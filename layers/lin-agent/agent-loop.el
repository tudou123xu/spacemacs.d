;;; agent-loop.el --- Claude agent core loop for Spacemacs. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Core agent loop: input → API → tool_use → execute → loop

(require 'json)
(require 'cl-lib)
(require 'subr-x)

;; ==================== Session State ====================

(defvar lin-agent--messages nil
  "Message history for the current session.")

(defvar lin-agent--turn-count 0
  "Number of turns in the current session.")

(defvar lin-agent--buffer-name "*Claude Agent*"
  "Buffer name for agent output.")

;; ==================== Read-only Tool Classification ====================

(defun lin-agent--tool-readonly-p (name)
  "Return non-nil if tool NAME is registered as read-only."
  (when (boundp 'lin-agent--tools)
    (let ((tool (gethash name lin-agent--tools)))
      (and tool (plist-get tool :read-only)))))

;; ==================== API Communication ====================

(defun lin-agent--api-headers ()
  "Build HTTP headers for the current provider."
  (let ((api-key (lin-ai/get-key lin-agent-provider))
        (api-type (lin-agent--current-api-type)))
    (unless api-key
      (lin-agent--display "\n[API key missing for %s. Use /key to set it]\n" lin-agent-provider)
      (user-error "API key not found for %s. Use /key to set it" lin-agent-provider))
    (pcase api-type
      ('claude
       `(("Content-Type" . "application/json")
         ("x-api-key" . ,api-key)
         ("anthropic-version" . ,lin-agent-api-version)))
      ('openai
       `(("Content-Type" . "application/json")
         ("Authorization" . ,(format "Bearer %s" api-key))))
      (_ (error "Unknown api-type: %s" api-type)))))

(defun lin-agent--build-request (messages &optional tools)
  "Build API request body from MESSAGES and optional TOOLS.
Adapts format based on current provider's api-type."
  (let ((system-prompt (if (fboundp 'lin-agent--build-system-prompt)
                           (lin-agent--build-system-prompt)
                         lin-agent-system-prompt))
        (api-type (lin-agent--current-api-type))
        (model (lin-agent--current-model)))
    (pcase api-type
      ('claude
       (let ((body `(("model" . ,model)
                     ("max_tokens" . ,lin-agent-max-tokens)
                     ("system" . ,system-prompt)
                     ("messages" . ,(vconcat messages)))))
         (when tools
           (push `("tools" . ,(vconcat tools)) body))
         body))
      ('openai
       (let* ((sys-msg `(("role" . "system") ("content" . ,system-prompt)))
              (converted (mapcar #'lin-agent--to-openai-msg messages))
              (flat-msgs (apply #'append
                                (mapcar (lambda (m)
                                          (if (and (listp m) (assoc "role" m))
                                              (list m)
                                            m))
                                        converted)))
              (oai-msgs (vconcat (cons sys-msg flat-msgs)))
              (body `(("model" . ,model)
                      ("max_tokens" . ,lin-agent-max-tokens)
                      ("messages" . ,oai-msgs))))
         (when tools
           (push `("tools" . ,(vconcat (mapcar #'lin-agent--to-openai-tool tools))) body))
         body)))))

(defun lin-agent--to-openai-msg (msg)
  "Convert a Claude-format MSG to OpenAI format.
Returns a single message alist or a LIST of messages (for tool_results)."
  (let ((role (alist-get 'role msg))
        (content (alist-get 'content msg)))
    (cond
     ;; Simple text user message
     ((and (equal role "user") (stringp content))
      `(("role" . "user") ("content" . ,content)))
     ;; User message with tool_result blocks → expand to multiple tool messages
     ((and (equal role "user") (or (vectorp content) (listp content)))
      (let ((parts (append (if (vectorp content) (append content nil) content) nil)))
        (if (cl-some (lambda (b) (equal (alist-get 'type b) "tool_result")) parts)
            (let ((results nil))
              (dolist (block parts)
                (when (equal (alist-get 'type block) "tool_result")
                  (push `(("role" . "tool")
                          ("tool_call_id" . ,(alist-get 'tool_use_id block))
                          ("content" . ,(let ((c (alist-get 'content block)))
                                          (if (stringp c) c (or (json-encode c) "")))))
                        results)))
              (nreverse results))
          `(("role" . "user") ("content" . ,(json-encode content))))))
     ;; Assistant message with tool_use
     ((and (equal role "assistant") (or (vectorp content) (listp content)))
      (let* ((parts (append (if (vectorp content) (append content nil) content) nil))
             (text-parts (cl-remove-if-not (lambda (b) (equal (alist-get 'type b) "text")) parts))
             (tool-parts (cl-remove-if-not (lambda (b) (equal (alist-get 'type b) "tool_use")) parts))
             (text (mapconcat (lambda (b) (or (alist-get 'text b) "")) text-parts ""))
             (tool-calls (mapcar (lambda (tu)
                                   `(("id" . ,(alist-get 'id tu))
                                     ("type" . "function")
                                     ("function" . (("name" . ,(alist-get 'name tu))
                                                    ("arguments" . ,(json-encode (alist-get 'input tu)))))))
                                 tool-parts)))
        (let ((m `(("role" . "assistant"))))
          (when (not (string-empty-p text))
            (push `("content" . ,text) m))
          (when tool-calls
            (push `("tool_calls" . ,(vconcat tool-calls)) m))
          m)))
     ;; Simple assistant text
     ((equal role "assistant")
      `(("role" . "assistant") ("content" . ,(or content ""))))
     ;; Fallback
     (t `(("role" . ,role) ("content" . ,(if (stringp content) content (json-encode content))))))))

(defun lin-agent--to-openai-tool (tool-def)
  "Convert Claude tool definition to OpenAI function calling format."
  (let ((name (alist-get "name" tool-def nil nil #'equal))
        (desc (alist-get "description" tool-def nil nil #'equal))
        (schema (alist-get "input_schema" tool-def nil nil #'equal)))
    `(("type" . "function")
      ("function" . (("name" . ,name)
                     ("description" . ,desc)
                     ("parameters" . ,schema))))))

(defvar lin-agent--max-retries 3)
(defvar lin-agent--busy nil)
(defvar lin-agent--retry-count 0)

(defun lin-agent--set-busy (busy)
  "Set BUSY state and update mode line."
  (setq lin-agent--busy busy)
  (when (get-buffer lin-agent--buffer-name)
    (with-current-buffer lin-agent--buffer-name
      (setq mode-line-process (if busy " [Thinking...]" nil))
      (force-mode-line-update))))

(defun lin-agent--json-read-with-symbols ()
  "Read JSON from current buffer with symbol keys and alist output."
  (let ((json-object-type 'alist)
        (json-key-type 'symbol)
        (json-array-type 'list))
    (json-read)))

(defun lin-agent--call-api (messages &optional tools)
  "Call Claude API synchronously with retry + exponential backoff."
  (require 'plz)
  (let ((retries 0)
        (body (lin-agent--build-request messages tools))
        response)
    (while (null response)
      (condition-case err
          (progn
            (setq response
                  (plz 'post (lin-agent--current-endpoint)
                    :headers (lin-agent--api-headers)
                    :body (json-encode body)
                    :as #'lin-agent--json-read-with-symbols
                    :timeout 180))
            (when (fboundp 'lin-agent--track-cost)
              (lin-agent--track-cost response)))
        (error
         (cl-incf retries)
         (if (> retries lin-agent--max-retries)
             (signal (car err) (cdr err))
           (let ((delay (min 30 (* (expt 2 (1- retries)) 1.0))))
             (lin-agent--display "\n  [API error: %s, retry %d/%d in %.0fs...]\n"
                                 (error-message-string err)
                                 retries lin-agent--max-retries delay)
             (sleep-for delay))))))
    response))

(defun lin-agent--call-api-async (messages tools callback &optional retry-count)
  "Call Claude API asynchronously. CALLBACK receives parsed response."
  (require 'plz)
  (let ((retries (or retry-count 0))
        (body (lin-agent--build-request messages tools)))
    (lin-agent--set-busy t)
    (plz 'post (lin-agent--current-endpoint)
      :headers (lin-agent--api-headers)
      :body (json-encode body)
      :as #'lin-agent--json-read-with-symbols
      :timeout 180
      :then (lambda (response)
              (lin-agent--set-busy nil)
              (when (fboundp 'lin-agent--track-cost)
                (lin-agent--track-cost response))
              (funcall callback response))
      :else (lambda (err)
              (let* ((next-retry (1+ retries))
                     (msg (lin-agent--plz-error-to-string err)))
                (if (> next-retry lin-agent--max-retries)
                    (progn
                      (lin-agent--set-busy nil)
                      (lin-agent--display "\n[API Error after %d retries: %s]\n"
                                          next-retry msg))
                  (let ((delay (min 30 (* (expt 2 (1- next-retry)) 1.0))))
                    (lin-agent--display "\n  [API error: %s, retry %d/%d in %.0fs...]\n"
                                        msg next-retry lin-agent--max-retries delay)
                    (run-at-time delay nil
                                 #'lin-agent--call-api-async
                                 messages tools callback next-retry))))))))

(defun lin-agent--plz-error-to-string (err)
  "Convert plz error ERR to human-readable string."
  (cond
   ((stringp err) err)
   ((and (listp err) (plist-get err :message))
    (plist-get err :message))
   ((and (recordp err) (fboundp 'plz-error-message))
    (condition-case nil
        (plz-error-message err)
      (error (format "%S" err))))
   (t (format "%S" err))))

;; ==================== Message Helpers ====================

(defun lin-agent--make-user-msg (text)
  "Create a user message alist."
  `((role . "user")
    (content . ,text)))

(defun lin-agent--make-tool-result (tool-use-id result &optional is-error)
  "Create a tool_result content block."
  `((type . "tool_result")
    (tool_use_id . ,tool-use-id)
    (content . ,(if (stringp result) result (json-encode result)))
    ,@(when is-error '((is_error . t)))))

(defun lin-agent--extract-text (response)
  "Extract text content from API RESPONSE."
  (let ((content (alist-get 'content response)))
    (mapconcat
     (lambda (block)
       (when (equal (alist-get 'type block) "text")
         (or (alist-get 'text block) "")))
     content "")))

(defun lin-agent--extract-tool-uses (response)
  "Extract tool_use blocks from API RESPONSE."
  (let ((content (alist-get 'content response)))
    (cl-remove-if-not
     (lambda (block) (equal (alist-get 'type block) "tool_use"))
     (append content nil))))

;; ==================== Core Agent Loop ====================

(defun lin-agent--loop (messages tools)
  "Core agent loop (async). Calls Claude, executes tools, loops until done."
  (if lin-agent--busy
      (message "Agent is busy, please wait...")
    (lin-agent--display "\n─── Turn %d [%s] ───\n"
                        (cl-incf lin-agent--turn-count)
                        (or (bound-and-true-p lin-agent--mode) 'agent))

    ;; Micro-compact old tool results
    (when (and (fboundp 'lin-agent--should-micro-compact-p)
               (lin-agent--should-micro-compact-p messages))
      (setq messages (lin-agent--micro-compact messages)))

    ;; Full auto-compact when context too large
    (when (and (fboundp 'lin-agent--should-compact-p)
               (lin-agent--should-compact-p messages))
      (lin-agent--display "  [Auto-compacting context...]\n")
      (setq messages (lin-agent--compact messages)))

    (lin-agent--call-api-async messages tools
      (lambda (response)
        (lin-agent--process-response response messages tools)))))

(defun lin-agent--normalize-response (response)
  "Normalize RESPONSE from any provider into Claude-like format.
Returns alist with keys: content (list of blocks), stop_reason, error, usage."
  (if (eq (lin-agent--current-api-type) 'openai)
      ;; OpenAI format → Claude format
      (let* ((err-obj (alist-get 'error response))
             (choices (alist-get 'choices response))
             (choice (and choices (elt choices 0)))
             (message (alist-get 'message choice))
             (finish (alist-get 'finish_reason choice))
             (text (alist-get 'content message))
             (tool-calls (alist-get 'tool_calls message))
             (content nil))
        ;; Build content blocks
        (when (and text (not (equal text :null)) (stringp text) (not (string-empty-p text)))
          (push `((type . "text") (text . ,text)) content))
        (when tool-calls
          (dolist (tc (append tool-calls nil))
            (let* ((fn (alist-get 'function tc))
                   (args-str (alist-get 'arguments fn))
                   (args (condition-case nil
                             (let ((json-object-type 'alist)
                                   (json-key-type 'symbol))
                               (json-read-from-string args-str))
                           (error nil))))
              (push `((type . "tool_use")
                      (id . ,(alist-get 'id tc))
                      (name . ,(alist-get 'name fn))
                      (input . ,args))
                    content))))
        (let ((stop-reason (pcase finish
                             ("stop" "end_turn")
                             ("tool_calls" "tool_use")
                             ("length" "max_tokens")
                             (_ (or finish "end_turn")))))
          `((content . ,(nreverse content))
            (stop_reason . ,stop-reason)
            ,@(when err-obj `((error . ,err-obj)))
            ,@(when-let ((usage (alist-get 'usage response)))
                `((usage . ,usage))))))
    ;; Already Claude format
    response))

(defun lin-agent--process-response (response messages tools)
  "Process API RESPONSE and continue the agent loop if needed."
  (let ((response (lin-agent--normalize-response response)))
  (condition-case err
      (let ((stop-reason (alist-get 'stop_reason response))
            (content (alist-get 'content response))
            (err-msg (alist-get 'error response)))

        ;; Handle API-level errors
        (when err-msg
          (lin-agent--display "\n[API Error: %s]\n"
                              (or (alist-get 'message err-msg)
                                  (format "%S" err-msg)))
          (cl-return-from lin-agent--process-response nil))

        ;; Display text
        (let ((text (lin-agent--extract-text response)))
          (when (and text (not (string-empty-p text)))
            (lin-agent--display "%s\n" text)))
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
                               (format "Tool '%s' blocked in %s mode."
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
           ;; Run turn-complete hooks
           (dolist (hook (or (bound-and-true-p lin-agent-turn-complete-hooks) nil))
             (ignore-errors (funcall hook lin-agent--messages)))
           ;; Re-insert input prompt for next message
           (when (get-buffer lin-agent--buffer-name)
             (with-current-buffer lin-agent--buffer-name
               (when (fboundp 'lin-agent--insert-prompt)
                 (lin-agent--insert-prompt))))
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
     (lin-agent--display "\n[Response Processing Error: %s]\n"
                         (error-message-string err))))))

;; ==================== Tool Hooks ====================

(defvar lin-agent-pre-tool-hooks nil)
(defvar lin-agent-post-tool-hooks nil)

(defun lin-agent--run-pre-hooks (name input)
  (cl-some (lambda (hook) (funcall hook name input)) lin-agent-pre-tool-hooks))

(defun lin-agent--run-post-hooks (name input result)
  (dolist (hook lin-agent-post-tool-hooks)
    (condition-case nil (funcall hook name input result) (error nil))))

;; ==================== Smart Tool Execution ====================

(defvar lin-agent--no-parallel-tools
  '("memory_search" "memory_kg_query" "memory_store"))

(defvar lin-agent--parallel-safe-tools
  '("file_read" "grep_search" "glob_search" "list_dir" "project_structure"
    "tool_search" "task_list" "task_get" "lsp_symbols")
  "Tools safe for parallel execution (no side effects).")

(defun lin-agent--execute-tools-smart (tool-uses)
  "Execute TOOL-USES, parallelizing when safe."
  (if (and (> (length tool-uses) 1)
           (cl-every (lambda (tu)
                       (let ((name (alist-get 'name tu)))
                         (or (lin-agent--tool-readonly-p name)
                             (member name lin-agent--parallel-safe-tools))
                         ))
                     tool-uses)
           (cl-notany (lambda (tu)
                        (member (alist-get 'name tu) lin-agent--no-parallel-tools))
                      tool-uses))
      (progn
        (lin-agent--display "  [Parallel: %d tools]\n" (length tool-uses))
        (lin-agent--execute-tools-parallel tool-uses))
    (lin-agent--execute-tools tool-uses)))

(defun lin-agent--execute-tools-parallel (tool-uses)
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
        (dolist (thr threads) (thread-join thr))
        (append results nil))
    (lin-agent--execute-tools tool-uses)))

(defun lin-agent--execute-single-tool (tool-use)
  "Execute a single TOOL-USE and return tool_result."
  (let* ((id (alist-get 'id tool-use))
         (name (alist-get 'name tool-use))
         (input (alist-get 'input tool-use))
         (perm-mode (or (bound-and-true-p lin-agent-permission-mode) 'default))
         (tool-perm (alist-get (intern name) lin-agent-tool-permissions 'confirm))
         (effective-perm
          (pcase perm-mode
            ('bypass 'auto)
            ('accept-edits
             (if (member name '("file_write" "file_edit")) 'auto tool-perm))
            (_ tool-perm))))
    (let ((start-time (float-time)))
    (lin-agent--display "\n  [Tool: %s]\n" name)
    (prog1 (cond
     ((and (equal name "shell_exec")
           (lin-agent--shell-blocked-p input))
      (lin-agent--make-tool-result id "Command blocked by shell sandbox." t))
     ((lin-agent--run-pre-hooks name input)
      (lin-agent--make-tool-result id "Tool blocked by pre-execution hook." t))
     ((and (eq effective-perm 'confirm)
           (not (lin-agent--permission-prompt name input)))
      (lin-agent--make-tool-result id "Tool execution denied by user." t))
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
          id (format "Error: %s" (error-message-string err)) t)))))
      (lin-agent--display "  [%s: %.1fs]\n" name (- (float-time) start-time))))))

(defun lin-agent--execute-tools (tool-uses)
  (mapcar #'lin-agent--execute-single-tool tool-uses))

;; ==================== Result Spilling ====================

(defvar lin-agent-tool-result-spill-threshold 10240)

(defvar lin-agent--spill-dir
  (expand-file-name "agent-spill/" temporary-file-directory))

(defun lin-agent--maybe-spill-result (name result)
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
  "Dispatch tool NAME with INPUT to the registry."
  (let ((handler (lin-agent-tool-get-handler name)))
    (if handler
        (lin-agent--maybe-spill-result name (funcall handler input))
      (format "Unknown tool: %s" name))))

;; ==================== Permission Prompt ====================

(defun lin-agent--permission-prompt (name input)
  "Show a rich permission prompt for tool NAME with INPUT context.
Returns non-nil if the user approves."
  (let ((summary (lin-agent--permission-summary name input)))
    (y-or-n-p (format "Allow %s? %s " name summary))))

(defun lin-agent--permission-summary (name input)
  "Build a human-readable summary of what tool NAME will do with INPUT."
  (pcase name
    ("file_write"
     (format "[Write %d bytes → %s]"
             (length (or (alist-get 'content input) ""))
             (or (alist-get 'path input) "?")))
    ("file_edit"
     (format "[Edit %s]" (or (alist-get 'path input) "?")))
    ("shell_exec"
     (format "[$ %s]"
             (truncate-string-to-width
              (or (alist-get 'command input) "?") 60 nil nil "...")))
    ("agent"
     (format "[Subagent: %s]"
             (truncate-string-to-width
              (or (alist-get 'task input) "?") 50 nil nil "...")))
    ("notebook_edit"
     (format "[.ipynb %s → %s]"
             (or (alist-get 'action input) "?")
             (truncate-string-to-width
              (or (alist-get 'notebook_path input) "?") 48 nil nil "...")))
    ("mcp_read_resource"
     (format "[MCP read %s → %s]"
             (or (alist-get 'server input) "?")
             (truncate-string-to-width
              (or (alist-get 'uri input) "?") 40 nil nil "...")))
    ("mcp_list_resources"
     (format "[MCP list %s]"
             (or (alist-get 'server input) "(all servers)")))
    (_
     (let ((keys (mapcar #'car input)))
       (format "[%s]" (mapconcat #'symbol-name (cl-subseq keys 0 (min 3 (length keys))) ", "))))))

;; ==================== Shell Sandbox ====================

(defun lin-agent--shell-blocked-p (input)
  (let ((command (alist-get 'command input))
        (cwd (alist-get 'cwd input)))
    (or
     (and command
          (bound-and-true-p lin-agent-shell-blocked-commands)
          (cl-some (lambda (pattern)
                     (string-match-p (regexp-quote pattern) command))
                   lin-agent-shell-blocked-commands))
     (and cwd
          (bound-and-true-p lin-agent-shell-allowed-dirs)
          lin-agent-shell-allowed-dirs
          (not (cl-some (lambda (dir)
                          (string-prefix-p (expand-file-name dir)
                                           (expand-file-name cwd)))
                        lin-agent-shell-allowed-dirs))))))

;; ==================== Display ====================

(defun lin-agent--display (fmt &rest args)
  "Append formatted text to the agent output buffer.
When the input prompt exists (markers valid), inserts text before the
prompt and re-inserts the prompt after. When markers are nil (e.g.
during streaming), simply appends text to the end."
  (let ((text (apply #'format fmt args)))
    (when (get-buffer lin-agent--buffer-name)
      (with-current-buffer lin-agent--buffer-name
        (let ((inhibit-read-only t)
              (has-prompt (and (local-variable-p 'lin-agent--prompt-start-marker)
                               lin-agent--prompt-start-marker
                               (marker-position lin-agent--prompt-start-marker)
                               lin-agent--input-marker
                               (marker-position lin-agent--input-marker))))
          (if has-prompt
              ;; Prompt exists: delete prompt+input, insert text, re-insert prompt
              (let ((prompt-start (marker-position lin-agent--prompt-start-marker))
                    (saved-input (buffer-substring-no-properties
                                  lin-agent--input-marker (point-max))))
                (delete-region prompt-start (point-max))
                (when (> (point-max) (point-min))
                  (remove-text-properties
                   (max (point-min) (1- (point-max))) (point-max) '(read-only nil)))
                (goto-char (point-max))
                (insert text)
                ;; Re-insert prompt on a new line
                (unless (or (bobp) (eq (char-before) ?\n))
                  (insert "\n"))
                (set-marker lin-agent--prompt-start-marker (point))
                (insert lin-agent--input-prompt)
                (add-text-properties (point-min) (point)
                                     '(read-only t rear-nonsticky t))
                (set-marker lin-agent--input-marker (point))
                (set-marker-insertion-type lin-agent--input-marker nil)
                (when (and saved-input (not (string-empty-p saved-input)))
                  (insert saved-input))
                (goto-char (point-max)))
            ;; No prompt (markers nil) — simple append
            (goto-char (point-max))
            (insert text)
            (goto-char (point-max)))))))
  (lin-agent--scroll-to-bottom))

(defun lin-agent--scroll-to-bottom ()
  "Scroll the agent buffer window to show latest output."
  (when-let ((win (get-buffer-window lin-agent--buffer-name t)))
    (with-selected-window win
      (goto-char (point-max))
      (recenter -3))))

;; ==================== Loop Dispatch ====================

(defvar lin-agent-streaming t
  "When non-nil, use SSE streaming for API calls.")

(defun lin-agent--loop-dispatch (messages tools)
  "Dispatch to streaming or non-streaming loop based on config."
  (if (and lin-agent-streaming (executable-find "curl"))
      (progn
        (require 'lin-agent-stream)
        (lin-agent--loop-streaming messages tools))
    (lin-agent--loop messages tools)))

(provide 'agent-loop)
;;; agent-loop.el ends here
