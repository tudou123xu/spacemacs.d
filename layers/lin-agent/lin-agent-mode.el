;;; lin-agent-mode.el --- Major mode for Claude Agent buffer. -*- lexical-binding: t; -*-
;; Author: xuzhifeng

(require 'cl-lib)

;; ==================== Font Lock ====================

(defvar lin-agent-mode-font-lock-keywords
  `(
    ;; Turn separators
    ("^─── .+ ───$" . font-lock-comment-face)
    ;; Section headers
    ("^\\*\\* \\(You\\)" 1 font-lock-keyword-face)
    ("^\\*\\* \\(Claude\\).*" 0 font-lock-function-name-face)
    ("^\\*\\* \\(Side Question\\)" 1 font-lock-keyword-face)
    ("^\\*\\* \\(Answer\\)" 1 font-lock-function-name-face)
    ;; Tool invocations
    ("\\[Tool: \\([a-z_]+\\)\\]" 1 font-lock-type-face)
    ("\\[Done: .*\\]" 0 font-lock-comment-face)
    ;; Status messages
    ("\\[Auto-\\(compacting\\|recalled\\|saved\\).*\\]" 0 font-lock-warning-face)
    ("\\[API error:.*\\]" 0 font-lock-warning-face)
    ("\\[Result too large.*\\]" 0 font-lock-warning-face)
    ("\\[Exported to .*\\]" 0 font-lock-string-face)
    ("\\[Session cleared\\]" 0 font-lock-warning-face)
    ("\\[Thinking\\.\\.\\.\\]" 0 font-lock-warning-face)
    ("\\[Model → .*\\]" 0 font-lock-string-face)
    ("\\[Output style → .*\\]" 0 font-lock-string-face)
    ("\\[Permission mode → .*\\]" 0 font-lock-string-face)
    ;; Org-style metadata
    ("^#\\+\\(TITLE\\|DATE\\|MODEL\\|MODE\\|SKILLS\\|MEMORY\\):.*" 0 font-lock-doc-face)
    ;; Slash commands in output
    ("^=== .+ ===$" 0 font-lock-constant-face)
    ;; Markdown-style headers in Claude output
    ("^### .+$" 0 font-lock-function-name-face)
    ("^## .+$" 0 font-lock-function-name-face)
    ("^# .+$" 0 font-lock-function-name-face)
    ;; Markdown code blocks (fenced)
    ("^```[a-z]*$" 0 font-lock-doc-face)
    ("^```$" 0 font-lock-doc-face)
    ;; Inline code `...`
    ("`\\([^`\n]+\\)`" 1 font-lock-constant-face)
    ;; Bold **text**
    ("\\*\\*\\([^*]+\\)\\*\\*" 1 '(:weight bold))
    ;; Italic *text*
    ("\\b\\*\\([^*\n]+\\)\\*\\b" 1 '(:slant italic))
    ;; Bullet points
    ("^[[:space:]]*[-*+] " 0 font-lock-keyword-face)
    ;; Numbered lists
    ("^[[:space:]]*[0-9]+\\. " 0 font-lock-keyword-face)
    ;; File paths
    ("/[^ \t\n]+\\.[a-z]+" 0 font-lock-string-face)
    ;; Cost display
    ("\\$[0-9]+\\.[0-9]+" 0 font-lock-constant-face)
    ;; Tokens display
    ("~?[0-9]+k? tokens?" 0 font-lock-constant-face)
    ;; URLs
    ("https?://[^ \t\n]+" 0 font-lock-string-face))
  "Font-lock keywords for lin-agent-mode.")

;; ==================== Input History ====================

(defvar lin-agent--input-ring nil
  "Ring buffer for input history.")

(defvar lin-agent--input-ring-size 100
  "Maximum number of inputs to remember.")

(defvar lin-agent--input-ring-index nil
  "Current position in input ring for navigation.")

(defun lin-agent--input-ring-init ()
  "Initialize the input ring if needed."
  (unless lin-agent--input-ring
    (setq lin-agent--input-ring (make-ring lin-agent--input-ring-size))))

(defun lin-agent--input-ring-push (input)
  "Push INPUT to the input history ring."
  (lin-agent--input-ring-init)
  (when (and input (not (string-empty-p input))
                   (not (string-prefix-p "/" input)))
    (ring-insert lin-agent--input-ring input)))

(defun lin-agent/previous-input ()
  "Navigate to the previous input in history."
  (interactive)
  (lin-agent--input-ring-init)
  (when (> (ring-length lin-agent--input-ring) 0)
    (setq lin-agent--input-ring-index
          (if lin-agent--input-ring-index
              (min (1+ lin-agent--input-ring-index)
                   (1- (ring-length lin-agent--input-ring)))
            0))
    (message "History [%d]: %s"
             lin-agent--input-ring-index
             (truncate-string-to-width
              (ring-ref lin-agent--input-ring lin-agent--input-ring-index) 80 nil nil "..."))))

(defun lin-agent/next-input ()
  "Navigate to the next input in history."
  (interactive)
  (lin-agent--input-ring-init)
  (when (and lin-agent--input-ring-index (> lin-agent--input-ring-index 0))
    (cl-decf lin-agent--input-ring-index)
    (message "History [%d]: %s"
             lin-agent--input-ring-index
             (truncate-string-to-width
              (ring-ref lin-agent--input-ring lin-agent--input-ring-index) 80 nil nil "..."))))

(defun lin-agent/send-from-history ()
  "Send the currently selected history item."
  (interactive)
  (lin-agent--input-ring-init)
  (if (and lin-agent--input-ring-index
           (< lin-agent--input-ring-index (ring-length lin-agent--input-ring)))
      (let ((input (ring-ref lin-agent--input-ring lin-agent--input-ring-index)))
        (setq lin-agent--input-ring-index nil)
        (lin-agent/send-message input))
    (call-interactively #'lin-agent/send-message)))

(defun lin-agent/send-with-history (prompt)
  "Send PROMPT and record in history."
  (interactive "sYou: ")
  (lin-agent--input-ring-push prompt)
  (setq lin-agent--input-ring-index nil)
  (lin-agent/send-message prompt))

;; ==================== Buffer Keymap ====================

(defvar lin-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'lin-agent/send-with-history)
    (define-key map (kbd "M-p") #'lin-agent/previous-input)
    (define-key map (kbd "M-n") #'lin-agent/next-input)
    (define-key map (kbd "M-RET") #'lin-agent/send-from-history)
    (define-key map (kbd "C-c C-c") #'lin-agent/send-with-history)
    (define-key map (kbd "C-c C-k") #'lin-agent/stop)
    (define-key map (kbd "C-c C-l") (lambda () (interactive) (lin-agent--try-slash-command "/clear")))
    (define-key map (kbd "C-c C-r") #'lin-agent/rewind)
    (define-key map (kbd "C-c C-s") #'lin-agent/save-session)
    (define-key map (kbd "C-c C-d") (lambda () (interactive) (lin-agent--try-slash-command "/doctor")))
    (define-key map (kbd "C-c C-e") (lambda () (interactive) (lin-agent--try-slash-command "/export")))
    (define-key map (kbd "C-c C-i") #'lin-agent/inspect-context)
    (define-key map (kbd "C-c C-w") #'lin-agent/copy-last-response)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `lin-agent-mode'.")

;; ==================== Major Mode ====================

(define-derived-mode lin-agent-mode special-mode "Claude-Agent"
  "Major mode for Claude Agent interaction buffer.

\\{lin-agent-mode-map}"
  :group 'lin-agent
  (setq-local font-lock-defaults '(lin-agent-mode-font-lock-keywords t))
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local buffer-read-only nil)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 2)
  (font-lock-mode 1))

;; ==================== Project Config ====================

(defvar lin-agent-project-config-names
  '(".lin-agent.md" ".claude.md" "CLAUDE.md")
  "File names to search for project-specific agent instructions.")

(defun lin-agent--load-project-config ()
  "Find and load project config (.lin-agent.md or .claude.md).
Returns the content string or nil."
  (let ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                  default-directory)))
    (cl-some
     (lambda (name)
       (let ((path (expand-file-name name root)))
         (when (file-exists-p path)
           (with-temp-buffer
             (insert-file-contents path)
             (buffer-string)))))
     lin-agent-project-config-names)))

(defun lin-agent--project-config-prompt ()
  "Build project config section for system prompt if config file exists."
  (when-let ((config (lin-agent--load-project-config)))
    (format "\n## Project Instructions\n%s" config)))

;; ==================== Copy Last Response ====================

(defun lin-agent/copy-last-response ()
  "Copy the last Claude response to kill ring."
  (interactive)
  (when (bound-and-true-p lin-agent--messages)
    (let ((last-msg (car (last lin-agent--messages))))
      (when last-msg
        (let ((content (alist-get 'content last-msg))
              (role (alist-get 'role last-msg)))
          (when (equal role "assistant")
            (let ((text (cond
                         ((stringp content) content)
                         ((vectorp content)
                          (mapconcat
                           (lambda (block)
                             (or (alist-get 'text block) ""))
                           content "")))))
              (when (and text (not (string-empty-p text)))
                (kill-new text)
                (message "Copied %d chars to kill ring." (length text))))))))))

;; ==================== Session Rename ====================

(defvar lin-agent--session-name nil
  "Custom name for the current session.")

(defun lin-agent/rename-session (name)
  "Rename the current agent session to NAME."
  (interactive "sSession name: ")
  (setq lin-agent--session-name name)
  (when (get-buffer lin-agent--buffer-name)
    (with-current-buffer lin-agent--buffer-name
      (rename-buffer (format "*Claude Agent: %s*" name) t)
      (setq lin-agent--buffer-name (buffer-name))))
  (message "Session renamed to: %s" name))

(provide 'lin-agent-mode)
;;; lin-agent-mode.el ends here
