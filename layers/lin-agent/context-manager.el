;;; context-manager.el --- Context assembly, modes, cost, persistence. -*- lexical-binding: t; -*-
;; Author: xuzhifeng

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;; ==================== Modes ====================

(defvar lin-agent--mode 'agent
  "Current agent mode: `agent', `plan', or `ask'.")

(defvar lin-agent-mode-prompts
  '((agent . "You are in AGENT mode. You can read files, write files, run commands, \
and use all tools. Execute tasks autonomously. Ask for confirmation before destructive operations.")
    (plan . "You are in PLAN mode (read-only). Analyze the codebase, explore options, \
and propose a plan. Do NOT modify any files. Only use read-only tools (file_read, grep_search, \
glob_find, list_directory, memory_search). Present 2-3 options with trade-offs.")
    (ask . "You are in ASK mode (read-only). Answer questions about the codebase. \
Do NOT modify any files. Only use read-only tools. Be concise and cite file paths."))
  "Mode-specific prompt fragments.")

(defun lin-agent/set-mode (mode)
  "Set agent mode to MODE (agent, plan, or ask)."
  (interactive
   (list (intern (completing-read "Mode: " '("agent" "plan" "ask") nil t))))
  (setq lin-agent--mode mode)
  (message "Agent mode: %s" mode))

;; ==================== Project Context ====================

(defun lin-agent--project-context ()
  "Build project context string from current environment."
  (let ((parts nil))
    (when-let ((root (and (fboundp 'projectile-project-root)
                          (projectile-project-root))))
      (push (format "Project root: %s" root) parts)
      (let ((project-type (and (fboundp 'projectile-project-type)
                               (projectile-project-type))))
        (when project-type
          (push (format "Project type: %s" project-type) parts))))

    (when buffer-file-name
      (push (format "Current file: %s (mode: %s, %d lines)"
                    buffer-file-name major-mode
                    (count-lines (point-min) (point-max)))
            parts))

    (when (and (fboundp 'vc-git-root) buffer-file-name)
      (when-let ((git-root (vc-git-root buffer-file-name)))
        (let ((branch (string-trim
                       (shell-command-to-string
                        (format "git -C %s rev-parse --abbrev-ref HEAD 2>/dev/null"
                                (shell-quote-argument git-root))))))
          (unless (string-empty-p branch)
            (push (format "Git branch: %s" branch) parts)))))

    (when parts
      (concat "\n## Environment\n"
              (mapconcat #'identity (nreverse parts) "\n")))))

;; ==================== System Prompt Assembly ====================

(defun lin-agent--build-system-prompt ()
  "Assemble full system prompt: base + mode + project + skills + tools."
  (let ((sections (list lin-agent-system-prompt)))
    ;; Mode
    (let ((mode-prompt (alist-get lin-agent--mode lin-agent-mode-prompts)))
      (when mode-prompt
        (push (format "\n## Current Mode: %s\n%s" lin-agent--mode mode-prompt) sections)))

    ;; Output style
    (when (and (boundp 'lin-agent-output-styles) (boundp 'lin-agent-output-style))
      (when-let ((style-prompt (alist-get lin-agent-output-style lin-agent-output-styles)))
        (push (format "\n## Output Style: %s\n%s" lin-agent-output-style style-prompt) sections)))

    ;; Project context
    (when-let ((ctx (lin-agent--project-context)))
      (push ctx sections))

    ;; Project config file (.lin-agent.md / .claude.md)
    (when (fboundp 'lin-agent--project-config-prompt)
      (when-let ((cfg (lin-agent--project-config-prompt)))
        (push cfg sections)))

    ;; Skills
    (when (fboundp 'lin-agent-skill--prompt-section)
      (when-let ((skills (lin-agent-skill--prompt-section)))
        (push skills sections)))

    ;; Available tools summary
    (when (fboundp 'lin-agent-tool-definitions)
      (let ((tool-names (mapcar (lambda (def)
                                  (alist-get "name" def nil nil #'equal))
                                (lin-agent-tool-definitions))))
        (push (format "\n## Available Tools\n%s"
                      (mapconcat #'identity tool-names ", "))
              sections)))

    (mapconcat #'identity (nreverse sections) "\n")))

;; ==================== Token Estimation ====================

(defun lin-agent--estimate-tokens (messages)
  "Rough token estimate for MESSAGES (4 chars ~ 1 token)."
  (let ((total 0))
    (dolist (msg messages)
      (let ((content (alist-get 'content msg)))
        (cond
         ((stringp content)
          (cl-incf total (/ (length content) 4)))
         ((vectorp content)
          (mapc (lambda (block)
                  (when-let ((text (or (alist-get 'text block)
                                       (alist-get 'content block))))
                    (when (stringp text)
                      (cl-incf total (/ (length text) 4)))))
                content)))))
    total))

;; ==================== Context Compaction ====================

(defvar lin-agent--compact-failures 0
  "Consecutive auto-compact failure count.")

(defvar lin-agent--compact-max-failures 3
  "Circuit breaker: stop auto-compacting after this many consecutive failures.")

(defun lin-agent--should-compact-p (messages)
  "Check if MESSAGES exceeds the auto-compact threshold.
Respects the circuit breaker after too many consecutive failures."
  (and (< lin-agent--compact-failures lin-agent--compact-max-failures)
       (> (lin-agent--estimate-tokens messages)
          lin-agent-auto-compact-threshold)))

(defun lin-agent--should-micro-compact-p (messages)
  "Check if micro-compact should run (halfway to full compact threshold)."
  (> (lin-agent--estimate-tokens messages)
     (/ lin-agent-auto-compact-threshold 2)))

(defun lin-agent--compact (messages)
  "Compact MESSAGES by summarizing older turns.
Keeps the first 2 messages and last N turns intact.
Tracks failures for circuit breaker."
  (let ((keep-last 6)
        (len (length messages)))
    (if (<= len (* keep-last 2))
        messages
      (let* ((head (cl-subseq messages 0 2))
             (tail (cl-subseq messages (- len keep-last)))
             (middle (cl-subseq messages 2 (- len keep-last)))
             (summary-text (format "[Context compacted: %d earlier messages summarized. \
~%dk tokens freed]"
                                   (length middle)
                                   (/ (lin-agent--estimate-tokens middle) 1000)))
             (summary-msg `((role . "user")
                            (content . ,summary-text))))
        (setq lin-agent--compact-failures 0)
        (append head (list summary-msg) tail)))))

(defun lin-agent/compact-context (&optional use-llm)
  "Manually compact the current session context.
With prefix arg USE-LLM, use the LLM to summarize."
  (interactive "P")
  (when lin-agent--messages
    (let ((before (lin-agent--estimate-tokens lin-agent--messages)))
      (if use-llm
          (lin-agent--compact-with-llm)
        (setq lin-agent--messages (lin-agent--compact lin-agent--messages)))
      (let ((after (lin-agent--estimate-tokens lin-agent--messages)))
        (message "Context compacted: ~%dk → ~%dk tokens" (/ before 1000) (/ after 1000))))))

;; ==================== Micro-compact (Incremental) ====================

(defvar lin-agent-micro-compact-turn-threshold 10
  "Number of turns before starting to fold old tool results.")

(defun lin-agent--micro-compact (messages)
  "Incrementally fold old tool results to reduce token usage.
Replaces tool_result content blocks older than THRESHOLD turns with summaries."
  (let* ((turn-count (length (cl-remove-if-not
                              (lambda (m) (equal (alist-get 'role m) "user"))
                              messages)))
         (fold-before (max 0 (- turn-count lin-agent-micro-compact-turn-threshold))))
    (if (<= fold-before 0)
        messages
      (let ((user-turn 0)
            (result nil))
        (dolist (msg messages)
          (when (equal (alist-get 'role msg) "user")
            (cl-incf user-turn))
          (if (and (< user-turn fold-before)
                   (equal (alist-get 'role msg) "user")
                   (vectorp (alist-get 'content msg)))
              ;; Fold tool_result blocks in old user messages
              (let ((folded-content
                     (vconcat
                      (mapcar
                       (lambda (block)
                         (if (equal (alist-get 'type block) "tool_result")
                             (let ((original (alist-get 'content block)))
                               (if (and (stringp original) (> (length original) 200))
                                   `((type . "tool_result")
                                     (tool_use_id . ,(alist-get 'tool_use_id block))
                                     (content . ,(format "[Folded: %d chars]"
                                                         (length original))))
                                 block))
                           block))
                       (append (alist-get 'content msg) nil)))))
                (push `((role . "user") (content . ,folded-content)) result))
            (push msg result)))
        (nreverse result)))))

;; ==================== LLM-based Compaction ====================

(defun lin-agent--compact-with-llm ()
  "Use the LLM itself to summarize older conversation turns."
  (require 'plz)
  (require 'agent-loop)
  (let* ((keep-last 4)
         (len (length lin-agent--messages))
         (to-summarize (if (> len (* keep-last 2))
                           (cl-subseq lin-agent--messages 0 (- len keep-last))
                         lin-agent--messages))
         (tail (if (> len (* keep-last 2))
                   (cl-subseq lin-agent--messages (- len keep-last))
                 nil))
         (summary-prompt (format "Summarize the following conversation concisely. \
Preserve: key decisions, file paths, code changes, user preferences, errors encountered. \
Be under 500 words.\n\n%s"
                                 (lin-agent--messages-to-text to-summarize)))
         (response (condition-case nil
                       (plz 'post lin-agent-api-endpoint
                         :headers (lin-agent--api-headers)
                         :body (json-encode
                                `(("model" . ,lin-agent-model)
                                  ("max_tokens" . 1024)
                                  ("messages" . ,(vector
                                                  `(("role" . "user")
                                                    ("content" . ,summary-prompt))))))
                         :as #'lin-agent--json-read-with-symbols
                         :timeout 60)
                     (error nil)))
         (summary (when response (lin-agent--extract-text response))))
    (when (fboundp 'lin-agent--track-cost)
      (lin-agent--track-cost response))
    (if (and summary (not (string-empty-p summary)))
        (let ((summary-msg `((role . "user")
                             (content . ,(format "[LLM Context Summary]\n%s" summary)))))
          (setq lin-agent--messages (append (list summary-msg) tail))
          (setq lin-agent--compact-failures 0))
      (setq lin-agent--messages (lin-agent--compact lin-agent--messages)))))

(defun lin-agent--messages-to-text (messages)
  "Convert MESSAGES list to readable text for summarization."
  (mapconcat
   (lambda (msg)
     (let ((role (alist-get 'role msg))
           (content (alist-get 'content msg)))
       (format "[%s]: %s"
               (upcase (or role "?"))
               (cond
                ((stringp content) (substring content 0 (min 500 (length content))))
                ((vectorp content)
                 (mapconcat
                  (lambda (block)
                    (or (alist-get 'text block)
                        (format "<%s>" (or (alist-get 'type block) "block"))))
                  content " "))
                (t "...")))))
   messages "\n"))

;; ==================== Cost Tracking ====================

(defvar lin-agent--cost-data nil
  "Session cost tracking: plist of (:input-tokens :output-tokens :total-cost).")

(defun lin-agent--reset-cost ()
  "Reset cost tracking for new session."
  (setq lin-agent--cost-data
        (list :input-tokens 0 :output-tokens 0 :total-cost 0.0)))

(defun lin-agent--track-cost (response)
  "Update cost data from API RESPONSE usage field."
  (when-let ((usage (alist-get 'usage response)))
    (let ((in-tok (or (alist-get 'input_tokens usage) 0))
          (out-tok (or (alist-get 'output_tokens usage) 0)))
      (setq lin-agent--cost-data
            (plist-put lin-agent--cost-data :input-tokens
                       (+ (plist-get lin-agent--cost-data :input-tokens) in-tok)))
      (setq lin-agent--cost-data
            (plist-put lin-agent--cost-data :output-tokens
                       (+ (plist-get lin-agent--cost-data :output-tokens) out-tok)))
      ;; Sonnet 4 pricing: $3/M in, $15/M out
      (let ((cost (+ (* in-tok 3.0 1e-6) (* out-tok 15.0 1e-6))))
        (setq lin-agent--cost-data
              (plist-put lin-agent--cost-data :total-cost
                         (+ (plist-get lin-agent--cost-data :total-cost) cost)))))))

(defun lin-agent/show-cost ()
  "Display session cost summary."
  (interactive)
  (if lin-agent--cost-data
      (message "Session cost: $%.4f | Input: %d tokens | Output: %d tokens | Turns: %d"
               (plist-get lin-agent--cost-data :total-cost)
               (plist-get lin-agent--cost-data :input-tokens)
               (plist-get lin-agent--cost-data :output-tokens)
               lin-agent--turn-count)
    (message "No active session.")))

;; ==================== Session Persistence ====================

;; ==================== Session Persistence (JSONL) ====================

(defvar lin-agent-session-dir
  (expand-file-name "agent-sessions/" user-emacs-directory)
  "Directory for session transcripts.")

(defvar lin-agent--session-id nil
  "Current session ID.")

(defvar lin-agent--session-file nil
  "Path to current session JSONL file.")

(defun lin-agent--session-init ()
  "Initialize session persistence for a new session."
  (make-directory lin-agent-session-dir t)
  (setq lin-agent--session-id (format-time-string "%Y%m%d-%H%M%S"))
  (setq lin-agent--session-file
        (expand-file-name (format "%s.jsonl" lin-agent--session-id)
                          lin-agent-session-dir))
  ;; Write header event
  (lin-agent--session-append-event
   "session_start"
   `((model . ,lin-agent-model)
     (mode . ,(symbol-name lin-agent--mode))
     (cwd . ,default-directory)
     (emacs_version . ,emacs-version))))

(defun lin-agent--session-append-event (type data)
  "Append a JSONL event of TYPE with DATA to the session file."
  (when lin-agent--session-file
    (let ((event `((type . ,type)
                   (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                   (data . ,data))))
      (with-temp-buffer
        (insert (json-encode event) "\n")
        (append-to-file (point-min) (point-max) lin-agent--session-file)))))

(defun lin-agent--session-append-message (msg)
  "Append a conversation MSG to the session transcript."
  (lin-agent--session-append-event "message" msg))

(defun lin-agent/save-session ()
  "Save current session (flush all messages to JSONL)."
  (interactive)
  (when lin-agent--messages
    (unless lin-agent--session-file
      (lin-agent--session-init))
    (lin-agent--session-append-event "save"
      `((turns . ,lin-agent--turn-count)
        (messages_count . ,(length lin-agent--messages))
        (cost . ,(when lin-agent--cost-data
                   (plist-get lin-agent--cost-data :total-cost)))))
    (message "Session saved: %s" lin-agent--session-file)))

(defun lin-agent/resume-session ()
  "Resume a saved session from JSONL transcript."
  (interactive)
  (make-directory lin-agent-session-dir t)
  (let* ((files (directory-files lin-agent-session-dir nil "\\.jsonl$"))
         (choice (completing-read "Resume session: " files nil t)))
    (when choice
      (let* ((path (expand-file-name choice lin-agent-session-dir))
             (messages nil)
             (meta nil))
        ;; Parse JSONL: read line by line
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((line (buffer-substring-no-properties
                          (point) (line-end-position)))
                   (json-object-type 'alist)
                   (json-key-type 'symbol)
                   (json-array-type 'list))
              (when (not (string-empty-p line))
                (condition-case nil
                    (let* ((event (json-read-from-string line))
                           (type (alist-get 'type event))
                           (data (alist-get 'data event)))
                      (cond
                       ((equal type "session_start")
                        (setq meta data))
                       ((equal type "message")
                        (push data messages))))
                  (error nil))))
            (forward-line 1)))
        ;; Restore state
        (setq lin-agent--messages (nreverse messages))
        (setq lin-agent--turn-count
              (/ (length (cl-remove-if-not
                          (lambda (m) (equal (alist-get 'role m) "assistant"))
                          lin-agent--messages))
                 1))
        (when meta
          (setq lin-agent-model (or (alist-get 'model meta) lin-agent-model))
          (setq lin-agent--mode (intern (or (alist-get 'mode meta) "agent"))))
        (lin-agent--reset-cost)
        (setq lin-agent--session-id (file-name-sans-extension choice))
        (setq lin-agent--session-file path)
        (lin-agent/start-session t)
        (message "Resumed session %s (%d messages, mode: %s)"
                 choice (length lin-agent--messages) lin-agent--mode)))))

;; ==================== Rewind ====================

(defun lin-agent/rewind (&optional n)
  "Rewind the last N turns (user+assistant pairs) from conversation history.
Each turn is 2 messages (user + assistant). Defaults to 1."
  (interactive "p")
  (let ((count (or n 1))
        (pairs (* 2 (or n 1))))
    (if (and lin-agent--messages (>= (length lin-agent--messages) pairs))
        (progn
          (setq lin-agent--messages
                (cl-subseq lin-agent--messages 0 (- (length lin-agent--messages) pairs)))
          (cl-decf lin-agent--turn-count count)
          (when (< lin-agent--turn-count 0) (setq lin-agent--turn-count 0))
          (message "Rewound %d turn(s). %d messages remain." count (length lin-agent--messages)))
      (message "Not enough history to rewind %d turn(s)." count))))

;; ==================== Context Inspector ====================

(defun lin-agent/inspect-context ()
  "Display the full conversation context in a dedicated buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Agent Context*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Agent Context Inspector ===\n\n")
      (insert (format "Mode: %s | Turns: %d | Messages: %d\n"
                      lin-agent--mode lin-agent--turn-count
                      (length lin-agent--messages)))
      (insert (format "Est. tokens: ~%dk\n"
                      (/ (lin-agent--estimate-tokens lin-agent--messages) 1000)))
      (when lin-agent--cost-data
        (insert (format "Cost: $%.4f (in: %d, out: %d)\n"
                        (plist-get lin-agent--cost-data :total-cost)
                        (plist-get lin-agent--cost-data :input-tokens)
                        (plist-get lin-agent--cost-data :output-tokens))))
      (insert (format "Token budget: %d / %d (%.0f%% used)\n\n"
                      (lin-agent--estimate-tokens lin-agent--messages)
                      lin-agent-token-budget
                      (* 100.0 (/ (float (lin-agent--estimate-tokens lin-agent--messages))
                                  lin-agent-token-budget))))
      (insert "─── Messages ───\n\n")
      (let ((idx 0))
        (dolist (msg lin-agent--messages)
          (let ((role (alist-get 'role msg))
                (content (alist-get 'content msg)))
            (insert (format "--- [%d] %s ---\n" idx (upcase (or role "?"))))
            (cond
             ((stringp content)
              (insert (if (> (length content) 500)
                          (format "%s\n... [truncated, %d chars total]\n"
                                  (substring content 0 500) (length content))
                        (format "%s\n" content))))
             ((vectorp content)
              (mapc (lambda (block)
                      (let ((type (alist-get 'type block)))
                        (cond
                         ((equal type "text")
                          (let ((text (alist-get 'text block)))
                            (insert (if (> (length text) 300)
                                        (format "  [text] %s... [%d chars]\n"
                                                (substring text 0 300) (length text))
                                      (format "  [text] %s\n" text)))))
                         ((equal type "tool_use")
                          (insert (format "  [tool_use] %s(%s)\n"
                                          (alist-get 'name block)
                                          (let ((inp (alist-get 'input block)))
                                            (if inp (json-encode inp) "")))))
                         ((equal type "tool_result")
                          (let ((c (alist-get 'content block)))
                            (insert (format "  [tool_result] %s\n"
                                            (if (and (stringp c) (> (length c) 200))
                                                (format "%s... [%d]" (substring c 0 200) (length c))
                                              (or c ""))))))
                         (t (insert (format "  [%s]\n" type))))))
                    content))
             (t (insert (format "%s\n" content))))
            (insert "\n")
            (cl-incf idx))))
      (goto-char (point-min)))
    (display-buffer buf)))

;; ==================== Token Budget ====================

(defvar lin-agent-token-budget 200000
  "Token budget for the session. Claude Sonnet context window.")

(defun lin-agent--token-budget-status ()
  "Return a formatted string of token budget usage."
  (let* ((used (lin-agent--estimate-tokens (or lin-agent--messages nil)))
         (budget (max 1 lin-agent-token-budget))
         (pct (* 100.0 (/ (float used) budget))))
    (format "Tokens: ~%dk / %dk (%.0f%%)" (/ used 1000) (/ budget 1000) pct)))

(defun lin-agent/show-token-budget ()
  "Display token budget usage."
  (interactive)
  (message "%s" (lin-agent--token-budget-status)))

(provide 'context-manager)
;;; context-manager.el ends here
