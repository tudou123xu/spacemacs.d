;;; agent-loop.el --- Claude agent core loop for Spacemacs. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Aligns with Claude Code's query.ts: input → API → tool_use → execute → loop
;; Features: parallel safe-tool execution, cost tracking, mode awareness.

(require 'json)
(require 'cl-lib)
(require 'subr-x)

;; ==================== Session State ====================

(defvar lin-agent--messages nil
  "Message history for the current session (list of alists).")

(defvar lin-agent--turn-count 0
  "Number of turns in the current session.")

(defvar lin-agent--buffer-name "*Claude Agent*"
  "Buffer name for agent output.")

;; ==================== Read-only Tool Classification ====================

(defun lin-agent--tool-readonly-p (name)
  "Return non-nil if tool NAME is registered as read-only in the tool registry."
  (when (boundp 'lin-agent--tools)
    (let ((tool (gethash name lin-agent--tools)))
      (and tool (plist-get tool :read-only)))))

;; ==================== API Communication ====================

(defun lin-agent--api-headers ()
  "Build HTTP headers for Claude API."
  (let ((api-key (lin-ai/get-key 'claude)))
    (unless api-key
      (user-error "Claude API key not found. Set in .authinfo: machine api.anthropic.com login claude password YOUR_KEY"))
    `(("Content-Type" . "application/json")
      ("x-api-key" . ,api-key)
      ("anthropic-version" . ,lin-agent-api-version))))

(defun lin-agent--build-request (messages &optional tools)
  "Build API request body from MESSAGES and optional TOOLS list."
  (let ((system-prompt (if (fboundp 'lin-agent--build-system-prompt)
                           (lin-agent--build-system-prompt)
                         lin-agent-system-prompt)))
    (let ((body `(("model" . ,lin-agent-model)
                  ("max_tokens" . ,lin-agent-max-tokens)
                  ("system" . ,system-prompt)
                  ("messages" . ,(vconcat messages)))))
      (when tools
        (push `("tools" . ,(vconcat tools)) body))
      body)))

(defvar lin-agent--max-retries 3
  "Maximum retries for transient API errors (429, 529, connection).")

(defvar lin-agent--busy nil
  "Non-nil when an async API call is in flight.")

(defvar lin-agent--retry-count 0
  "Current retry count for the in-flight request.")

(defun lin-agent--set-busy (busy)
  "Set BUSY state and update mode line."
  (setq lin-agent--busy busy)
  (when (get-buffer lin-agent--buffer-name)
    (with-current-buffer lin-agent--buffer-name
      (setq mode-line-process (if busy " [Thinking...]" nil))
      (force-mode-line-update))))

(defun lin-agent--call-api (messages &optional tools)
  "Call Claude API synchronously with retry + exponential backoff.
Returns parsed JSON response with symbol keys."
  (require 'plz)
  (let ((retries 0)
        (json-object-type 'alist)
        (json-key-type 'symbol)
        (json-array-type 'list)
        (body (lin-agent--build-request messages tools))
        response)
    (while (null response)
      (condition-case err
          (progn
            (setq response
                  (plz 'post lin-agent-api-endpoint
                    :headers (lin-agent--api-headers)
                    :body (json-encode body)
                    :as #'json-read
                    :timeout 180))
            (when (fboundp 'lin-agent--track-cost)
              (lin-agent--track-cost response)))
        (error
         (cl-incf retries)
         (if (> retries lin-agent--max-retries)
             (signal (car err) (cdr err))
           (let ((delay (min 30 (* (expt 2 (1- retries)) 1.0))))
             (lin-agent--display "\n  [API error: %s, retry %d/%d in %.0fs...]\n"
                                 (error-message-string err) retries lin-agent--max-retries delay)
             (sleep-for delay))))))
    response))

(defun lin-agent--call-api-async (messages tools callback &optional retry-count)
  "Call Claude API asynchronously. CALLBACK receives (response).
RETRY-COUNT is the current retry attempt."
  (require 'plz)
  (let* ((retries (or retry-count 0))
         (json-object-type 'alist)
         (json-key-type 'symbol)
         (json-array-type 'list)
         (body (lin-agent--build-request messages tools)))
    (lin-agent--set-busy t)
    (plz 'post lin-agent-api-endpoint
      :headers (lin-agent--api-headers)
      :body (json-encode body)
      :as #'json-read
      :timeout 180
      :then (lambda (response)
              (lin-agent--set-busy nil)
              (when (fboundp 'lin-agent--track-cost)
                (lin-agent--track-cost response))
              (funcall callback response))
      :else (lambda (err)
              (let ((next-retry (1+ retries)))
                (if (> next-retry lin-agent--max-retries)
                    (progn
                      (lin-agent--set-busy nil)
                      (lin-agent--display "\n[API Error after %d retries: %s]\n"
                                          next-retry (plz-error-message err)))
                  (let ((delay (min 30 (* (expt 2 (1- next-retry)) 1.0))))
                    (lin-agent--display "\n  [API error: %s, retry %d/%d in %.0fs...]\n"
                                        (plz-error-message err)
                                        next-retry lin-agent--max-retries delay)
                    (run-at-time delay nil
                                 #'lin-agent--call-api-async
                                 messages tools callback next-retry))))))))

(defun plz-error-message (err)
  "Extract a human-readable message from a plz error ERR."
  (cond
   ((stringp err) err)
   ((and (listp err) (plist-get err :message)) (plist-get err :message))
   (t (format "%S" err))))

;; ==================== Message Helpers ====================

(defun lin-agent--make-user-msg (text)
  "Create a user message alist with symbol keys (consistent with json-read)."
  `((role . "user")
    (content . ,text)))

(defun lin-agent--make-tool-result (tool-use-id result &optional is-error)
  "Create a tool_result content block with symbol keys."
  `((type . "tool_result")
    (tool_use_id . ,tool-use-id)
    (content . ,(if (stringp result) result (json-encode result)))
    ,@(when is-error '((is_error . t)))))

(defun lin-agent--extract-text (response)
  "Extract text content from API RESPONSE."
  (let ((content (alist-get 'content response)))
    (mapconcat
     (lambda (block)
       (when (string= (alist-get 'type block) "text")
         (alist-get 'text block)))
     content "")))

(defun lin-agent--extract-tool-uses (response)
  "Extract tool_use blocks from API RESPONSE."
  (let ((content (alist-get 'content response)))
    (cl-remove-if-not
     (lambda (block) (string= (alist-get 'type block) "tool_use"))
     (append content nil))))

;; ==================== Core Agent Loop ====================

(defun lin-agent--loop (messages tools)
  "Core agent loop (async). Calls Claude, executes tools, loops until done.
MESSAGES is the conversation history.
TOOLS is the tool definitions list.
Supports mode-aware tool filtering and auto-compaction."
  (if lin-agent--busy
      (message "Agent is busy, please wait...")
    (lin-agent--display "\n─── Turn %d [%s] ───\n"
                        (cl-incf lin-agent--turn-count)
                        (or (bound-and-true-p lin-agent--mode) 'agent))

    ;; Micro-compact: fold old tool results first
    (when (and (fboundp 'lin-agent--should-micro-compact-p)
               (lin-agent--should-micro-compact-p messages))
      (setq messages (lin-agent--micro-compact messages)))

    ;; Full auto-compact when context is still too large
    (when (and (fboundp 'lin-agent--should-compact-p)
               (lin-agent--should-compact-p messages))
      (lin-agent--display "  [Auto-compacting context...]\n")
      (setq messages (lin-agent--compact messages)))

    (lin-agent--call-api-async messages tools
      (lambda (response)
        (lin-agent--process-response response messages tools)))))

(defun lin-agent--process-response (response messages tools)
  "Process API RESPONSE and continue the agent loop."
  (condition-case err
      (let ((stop-reason (alist-get 'stop_reason response))
            (content (alist-get 'content response)))
        ;; Display text
        (let ((text (lin-agent--extract-text response)))
          (when (and text (not (string-empty-p text)))
            (lin-agent--display "%s\n" text)))
        ;; Auto-scroll
        (lin-agent--scroll-to-bottom)

        (pcase stop-reason
          ("tool_use"
           (let* ((tool-uses (lin-agent--extract-tool-uses response))
                  (filtered-uses
                   (if (memq (bound-and-true-p lin-agent--mode) '(plan ask))
                       (cl-remove-if-not
                        (lambda (tu) (lin-agent--tool-readonly-p (alist-get 'name tu)))
                        tool-uses)
                     tool-uses))
                  (blocked (cl-set-difference tool-uses filtered-uses))
                  (assistant-msg `((role . "assistant")
                                   (content . ,(vconcat content))))
                  (results
                   (append
                    (lin-agent--execute-tools-smart filtered-uses)
                    (mapcar (lambda (tu)
                              (lin-agent--make-tool-result
                               (alist-get 'id tu)
                               (format "Tool '%s' blocked in %s mode (read-only)."
                                       (alist-get 'name tu) lin-agent--mode)
                               t))
                            blocked)))
                  (user-msg `((role . "user")
                              (content . ,(vconcat results)))))
             (lin-agent--loop
              (append messages (list assistant-msg user-msg))
              tools)))

          ("end_turn"
           (let ((assistant-msg `((role . "assistant")
                                  (content . ,(vconcat content)))))
             (setq lin-agent--messages (append messages (list assistant-msg))))
           (lin-agent--display "\n─── Session: %d turns" lin-agent--turn-count)
           (when (bound-and-true-p lin-agent--cost-data)
             (lin-agent--display " | $%.4f"
                                 (plist-get lin-agent--cost-data :total-cost)))
           (lin-agent--display " ───\n")
           (lin-agent--scroll-to-bottom))

          ("max_tokens"
           (lin-agent--display "\n[Reached max tokens, continuing...]\n")
           (let ((assistant-msg `((role . "assistant")
                                  (content . ,(vconcat content)))))
             (lin-agent--loop
              (append messages
                      (list assistant-msg
                            (lin-agent--make-user-msg "Continue.")))
              tools)))

          (_
           (lin-agent--display "\n[Unexpected stop_reason: %s]\n" stop-reason))))
    (error
     (lin-agent--display "\n[Response Processing Error: %s]\n" (error-message-string err)))))

;; ==================== Tool Hooks ====================

(defvar lin-agent-pre-tool-hooks nil
  "Hook list called before tool execution. Each hook receives (name input).
Return non-nil to skip the tool.")

(defvar lin-agent-post-tool-hooks nil
  "Hook list called after tool execution. Each hook receives (name input result).")

(defun lin-agent--run-pre-hooks (name input)
  "Run pre-tool hooks. Return non-nil if any hook vetoes execution."
  (cl-some (lambda (hook) (funcall hook name input)) lin-agent-pre-tool-hooks))

(defun lin-agent--run-post-hooks (name input result)
  "Run post-tool hooks."
  (dolist (hook lin-agent-post-tool-hooks)
    (condition-case nil
        (funcall hook name input result)
      (error nil))))

;; ==================== Smart Tool Execution ====================

(defvar lin-agent--no-parallel-tools
  '("memory_search" "memory_kg_query" "memory_store")
  "Tools that must NOT run in parallel (e.g. single MCP stdio connection).")

(defun lin-agent--execute-tools-smart (tool-uses)
  "Execute TOOL-USES with parallel execution for read-only non-MCP tools."
  (if (and (> (length tool-uses) 1)
           (cl-every (lambda (tu)
                       (let ((name (alist-get 'name tu)))
                         (and (lin-agent--tool-readonly-p name)
                              (not (member name lin-agent--no-parallel-tools)))))
                     tool-uses))
      (lin-agent--execute-tools-parallel tool-uses)
    (lin-agent--execute-tools tool-uses)))

(defun lin-agent--execute-tools-parallel (tool-uses)
  "Execute read-only TOOL-USES concurrently.
All tools in this path must be auto-permission (no interactive prompts).
Uses threads on Emacs 26+ with native threads, falls back to sequential."
  (if (and (fboundp 'make-thread) (> (length tool-uses) 1))
      (let* ((results (make-vector (length tool-uses) nil))
             (threads nil)
             (idx 0))
        (dolist (tool-use tool-uses)
          (let ((i idx)
                (tu tool-use))
            (push (make-thread
                   (lambda ()
                     (let* ((id (alist-get 'id tu))
                            (name (alist-get 'name tu))
                            (input (alist-get 'input tu)))
                       (if (lin-agent--run-pre-hooks name input)
                           (aset results i
                                 (lin-agent--make-tool-result id "Blocked by pre-hook." t))
                         (condition-case err
                             (let ((result (lin-agent--run-tool name input)))
                               (lin-agent--run-post-hooks name input result)
                               (aset results i (lin-agent--make-tool-result id result)))
                           (error
                            (aset results i
                                  (lin-agent--make-tool-result
                                   id (format "Error: %s" (error-message-string err)) t))))))))
                  threads)
            (cl-incf idx)))
        (dolist (thr threads)
          (thread-join thr))
        (append results nil))
    (lin-agent--execute-tools tool-uses)))

(defun lin-agent--execute-single-tool (tool-use)
  "Execute a single TOOL-USE and return tool_result.
Respects pre/post hooks and permission system."
  (let* ((id (alist-get 'id tool-use))
         (name (alist-get 'name tool-use))
         (input (alist-get 'input tool-use))
         (perm-mode (or (bound-and-true-p lin-agent-permission-mode) 'default))
         (tool-perm (alist-get (intern name) lin-agent-tool-permissions 'confirm))
         (effective-perm
          (pcase perm-mode
            ('bypass 'auto)
            ('accept-edits
             (if (member name '("file_write" "file_edit"))
                 'auto
               tool-perm))
            (_ tool-perm))))
    (lin-agent--display "\n  [Tool: %s]\n" name)
    (cond
     ;; Shell sandbox check
     ((and (equal name "shell_exec")
           (lin-agent--shell-blocked-p input))
      (lin-agent--make-tool-result id "Command blocked by shell sandbox." t))
     ;; Pre-hook veto
     ((lin-agent--run-pre-hooks name input)
      (lin-agent--make-tool-result id "Tool blocked by pre-execution hook." t))
     ;; Permission check
     ((and (eq effective-perm 'confirm)
           (not (y-or-n-p (format "Allow tool '%s'? " name))))
      (lin-agent--make-tool-result id "Tool execution denied by user." t))
     ;; Execute
     (t
      (condition-case err
          (let ((result (lin-agent--run-tool name input)))
            (lin-agent--run-post-hooks name input result)
            (lin-agent--display "  [Done: %s]\n"
                                (truncate-string-to-width
                                 (replace-regexp-in-string "\n" " " (format "%s" result))
                                 80 nil nil "..."))
            (lin-agent--make-tool-result id result))
        (error
         (lin-agent--make-tool-result
          id (format "Error: %s" (error-message-string err)) t)))))))

(defun lin-agent--execute-tools (tool-uses)
  "Execute TOOL-USES sequentially. Returns list of tool_result blocks."
  (mapcar #'lin-agent--execute-single-tool tool-uses))

(defvar lin-agent-tool-result-spill-threshold 10240
  "Byte threshold above which tool results are spilled to temp files.")

(defvar lin-agent--spill-dir
  (expand-file-name "agent-spill/" temporary-file-directory)
  "Directory for spilled tool results.")

(defun lin-agent--maybe-spill-result (name result)
  "If RESULT exceeds threshold, write to temp file and return a reference."
  (if (and (stringp result)
           (> (length result) lin-agent-tool-result-spill-threshold))
      (progn
        (make-directory lin-agent--spill-dir t)
        (let ((path (expand-file-name
                     (format "%s-%s.txt" name (format-time-string "%H%M%S"))
                     lin-agent--spill-dir)))
          (with-temp-file path (insert result))
          (format "[Result too large (%d bytes), saved to: %s]\n\nFirst 2000 chars:\n%s"
                  (length result) path (substring result 0 (min 2000 (length result))))))
    result))

(defun lin-agent--run-tool (name input)
  "Dispatch tool NAME with INPUT to the tool registry. Spills large results."
  (let ((handler (lin-agent-tool-get-handler name)))
    (if handler
        (lin-agent--maybe-spill-result name (funcall handler input))
      (format "Unknown tool: %s" name))))

;; ==================== Shell Sandbox ====================

(defun lin-agent--shell-blocked-p (input)
  "Check if shell INPUT should be blocked by sandbox rules."
  (let ((command (alist-get 'command input))
        (cwd (alist-get 'cwd input)))
    (or
     ;; Blocked command patterns
     (and command
          (bound-and-true-p lin-agent-shell-blocked-commands)
          (cl-some (lambda (pattern)
                     (string-match-p (regexp-quote pattern) command))
                   lin-agent-shell-blocked-commands))
     ;; Directory restriction
     (and cwd
          (bound-and-true-p lin-agent-shell-allowed-dirs)
          lin-agent-shell-allowed-dirs
          (not (cl-some (lambda (dir)
                          (string-prefix-p (expand-file-name dir) (expand-file-name cwd)))
                        lin-agent-shell-allowed-dirs))))))

;; ==================== Display ====================

(defun lin-agent--display (fmt &rest args)
  "Append formatted text to the agent output buffer."
  (let ((text (apply #'format fmt args)))
    (with-current-buffer (get-buffer-create lin-agent--buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)
        (goto-char (point-max))))
    (lin-agent--scroll-to-bottom)))

(defun lin-agent--scroll-to-bottom ()
  "Scroll the agent buffer window to show the latest output."
  (when-let ((win (get-buffer-window lin-agent--buffer-name t)))
    (with-selected-window win
      (goto-char (point-max))
      (recenter -3))))

(provide 'agent-loop)
;;; agent-loop.el ends here
