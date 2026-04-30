;;; lin-agent-mode.el --- Major mode for Claude Agent buffer. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Provides comint-style input area, font-lock, input history, Evil integration.

(require 'cl-lib)
(require 'subr-x)

;; ==================== Input Prompt ====================

(defvar lin-agent--input-prompt "❯ "
  "Prompt string shown before the input area.")

(defvar lin-agent--input-marker nil
  "Marker for the beginning of user input area. Buffer-local.")

(defvar lin-agent--prompt-start-marker nil
  "Marker for the beginning of the prompt string. Buffer-local.")

;; ==================== Custom Faces ====================

(defgroup lin-agent-faces nil
  "Faces for Lin Agent mode."
  :group 'lin-agent
  :prefix "lin-agent-")

(defface lin-agent-user-face
  '((t :foreground "#61afef" :weight bold))
  "Face for user messages.")

(defface lin-agent-assistant-face
  '((t :foreground "#c678dd" :weight bold))
  "Face for assistant messages.")

(defface lin-agent-tool-face
  '((t :foreground "#e5c07b"))
  "Face for tool invocations.")

(defface lin-agent-tool-done-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for tool completion notices.")

(defface lin-agent-separator-face
  '((t :foreground "#4b5263"))
  "Face for separator lines.")

(defface lin-agent-prompt-face
  '((t :foreground "#98c379" :weight bold))
  "Face for input prompt.")

(defface lin-agent-cost-face
  '((t :foreground "#d19a66"))
  "Face for cost/token displays.")

(defface lin-agent-error-face
  '((t :foreground "#e06c75" :weight bold))
  "Face for error messages.")

(defface lin-agent-header-face
  '((t :foreground "#56b6c2"))
  "Face for section headers.")

;; ==================== Font Lock ====================

(defvar lin-agent-mode-font-lock-keywords
  `(("^─── .+ ───$" 0 'lin-agent-separator-face)
    ("^\\*\\* \\(You\\)" 1 'lin-agent-user-face)
    ("^\\*\\* \\(Claude\\|Siliconflow\\|Deepseek\\|Openai\\).*" 0 'lin-agent-assistant-face)
    ("^\\*\\* \\(Side Question\\)" 1 'lin-agent-user-face)
    ("^\\*\\* \\(Answer\\)" 1 'lin-agent-assistant-face)
    ("\\[Tool: \\([a-z_]+\\)\\]" 0 'lin-agent-tool-face)
    ("\\[Parallel: [0-9]+ tools\\]" 0 'lin-agent-tool-face)
    ("\\[[a-z_]+: [0-9.]+s\\]" 0 'lin-agent-tool-done-face)
    ("\\[Done: .*\\]" 0 'lin-agent-tool-done-face)
    ("\\[Auto-\\(compacting\\|recalled\\|saved\\).*\\]" 0 'lin-agent-header-face)
    ("\\[API error:.*\\]" 0 'lin-agent-error-face)
    ("\\[Result too large.*\\]" 0 'lin-agent-error-face)
    ("\\[Stream Error:.*\\]" 0 'lin-agent-error-face)
    ("\\[Stream interrupted\\]" 0 'lin-agent-error-face)
    ("\\[Thinking\\.\\.\\.\\]" 0 'lin-agent-header-face)
    ("^#\\+\\(TITLE\\|DATE\\|PROVIDER\\|MODEL\\|MODE\\|SKILLS\\|MEMORY\\):.*" 0 font-lock-doc-face)
    ("^=== .+ ===$" 0 'lin-agent-header-face)
    ;; Markdown headers
    ("^### .+$" 0 'lin-agent-assistant-face)
    ("^## .+$" 0 'lin-agent-assistant-face)
    ("^# .+$" 0 'lin-agent-assistant-face)
    ;; Code fences
    ("^```[a-z]*$" 0 font-lock-doc-face)
    ("^```$" 0 font-lock-doc-face)
    ;; Inline code
    ("`\\([^`\n]+\\)`" 1 font-lock-constant-face)
    ;; Bold
    ("\\*\\*\\([^*]+\\)\\*\\*" 1 '(:weight bold))
    ;; Lists
    ("^[[:space:]]*[-*+] " 0 font-lock-keyword-face)
    ("^[[:space:]]*[0-9]+\\. " 0 font-lock-keyword-face)
    ;; Cost / token
    ("\\$[0-9]+\\.[0-9]+" 0 'lin-agent-cost-face)
    ("~?[0-9]+k? tokens?" 0 'lin-agent-cost-face)
    ;; URLs
    ("https?://[^ \t\n]+" 0 font-lock-string-face)
    ;; Input prompt
    ("^❯ " 0 'lin-agent-prompt-face)
    ;; Cursor-style @path
    ("@\\(?:\"[^\"]+\"\\|'[^']+'\\|[^ \t\n]+\\)" 0 'lin-agent-header-face))
  "Font-lock keywords for lin-agent-mode.")

;; ==================== Input History ====================

(defvar lin-agent--input-ring nil)
(defvar lin-agent--input-ring-size 100)
(defvar lin-agent--input-ring-index nil)

(defun lin-agent--input-ring-init ()
  (unless lin-agent--input-ring
    (setq lin-agent--input-ring (make-ring lin-agent--input-ring-size))))

(defun lin-agent--input-ring-push (input)
  (lin-agent--input-ring-init)
  (when (and input (not (string-empty-p input))
             (not (string-prefix-p "/" input)))
    (ring-insert lin-agent--input-ring input)))

(defun lin-agent/previous-input ()
  "Replace input area with previous history item."
  (interactive)
  (lin-agent--input-ring-init)
  (when (and lin-agent--input-marker
             (marker-position lin-agent--input-marker)
             (> (ring-length lin-agent--input-ring) 0))
    (setq lin-agent--input-ring-index
          (if lin-agent--input-ring-index
              (min (1+ lin-agent--input-ring-index)
                   (1- (ring-length lin-agent--input-ring)))
            0))
    (let ((text (ring-ref lin-agent--input-ring lin-agent--input-ring-index))
          (inhibit-read-only t))
      (delete-region lin-agent--input-marker (point-max))
      (goto-char (point-max))
      (insert text))))

(defun lin-agent/next-input ()
  "Replace input area with next history item."
  (interactive)
  (lin-agent--input-ring-init)
  (when (and lin-agent--input-marker
             (marker-position lin-agent--input-marker)
             lin-agent--input-ring-index)
    (if (> lin-agent--input-ring-index 0)
        (progn
          (cl-decf lin-agent--input-ring-index)
          (let ((text (ring-ref lin-agent--input-ring lin-agent--input-ring-index))
                (inhibit-read-only t))
            (delete-region lin-agent--input-marker (point-max))
            (goto-char (point-max))
            (insert text)))
      (let ((inhibit-read-only t))
        (delete-region lin-agent--input-marker (point-max))
        (setq lin-agent--input-ring-index nil)))))

;; ==================== Input Area Management ====================

(defun lin-agent--insert-prompt ()
  "Insert the input prompt at the end of the buffer.
Sets up read-only text properties on all content before the prompt,
leaving the area after the prompt writable for user input."
  (let ((inhibit-read-only t))
    ;; If a previous prompt exists, remove it precisely using prompt-start marker
    (when (and lin-agent--prompt-start-marker
               (marker-position lin-agent--prompt-start-marker))
      (delete-region (marker-position lin-agent--prompt-start-marker) (point-max)))
    ;; Fallback: use input-marker if prompt-start is missing
    (when (and (not (and lin-agent--prompt-start-marker
                         (marker-position lin-agent--prompt-start-marker)))
               lin-agent--input-marker
               (marker-position lin-agent--input-marker))
      (let ((old-start (save-excursion
                         (goto-char lin-agent--input-marker)
                         (line-beginning-position))))
        (delete-region old-start (point-max))))
    (goto-char (point-max))
    (unless (or (bobp) (eq (char-before) ?\n))
      (insert "\n"))
    ;; Record prompt start position
    (set-marker (or lin-agent--prompt-start-marker
                    (setq lin-agent--prompt-start-marker (point-marker)))
                (point))
    (insert lin-agent--input-prompt)
    (add-text-properties (point-min) (point)
                         '(read-only t rear-nonsticky t))
    (set-marker (or lin-agent--input-marker
                    (setq lin-agent--input-marker (point-marker)))
                (point))
    (set-marker-insertion-type lin-agent--input-marker nil)
    (goto-char (point-max))))

(defun lin-agent--get-input ()
  "Get text from the input area (after the prompt marker)."
  (when (and lin-agent--input-marker (marker-position lin-agent--input-marker))
    (buffer-substring-no-properties lin-agent--input-marker (point-max))))

(defun lin-agent--clear-input ()
  "Clear the input area and remove the prompt.
Invalidates markers so `lin-agent--display' uses simple append during streaming."
  (let ((inhibit-read-only t))
    (cond
     ;; Precise deletion using prompt-start marker
     ((and lin-agent--prompt-start-marker
           (marker-position lin-agent--prompt-start-marker))
      (delete-region (marker-position lin-agent--prompt-start-marker) (point-max)))
     ;; Fallback: use input-marker
     ((and lin-agent--input-marker
           (marker-position lin-agent--input-marker))
      (save-excursion
        (goto-char lin-agent--input-marker)
        (beginning-of-line)
        (delete-region (point) (point-max))))))
  ;; Invalidate both markers so streaming uses simple append path
  (when lin-agent--prompt-start-marker
    (set-marker lin-agent--prompt-start-marker nil))
  (when lin-agent--input-marker
    (set-marker lin-agent--input-marker nil)))

(defun lin-agent/submit-input ()
  "Submit the current input from the input area."
  (interactive)
  (let ((input (lin-agent--get-input)))
    (if (or (null input) (string-empty-p (string-trim input)))
        (message "Empty input. Type your message after the ❯ prompt.")
      ;; Clear input area
      (lin-agent--clear-input)
      ;; Reset history navigation
      (setq lin-agent--input-ring-index nil)
      ;; Send the message (which will re-insert prompt when done)
      (lin-agent/send-message input))))

(defun lin-agent/send-with-history (prompt)
  "Send PROMPT (from minibuffer) and record in history."
  (interactive "sYou: ")
  (when (and prompt (not (string-empty-p (string-trim prompt))))
    (lin-agent--input-ring-push prompt)
    (setq lin-agent--input-ring-index nil)
    (lin-agent/send-message prompt)))

;; ==================== Keymap ====================

(defvar lin-agent-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Primary: submit from input area
    (define-key map (kbd "RET")     #'lin-agent/submit-input)
    ;; Fallback: minibuffer input
    (define-key map (kbd "C-c C-c") #'lin-agent/send-with-history)
    ;; History navigation
    (define-key map (kbd "M-p")     #'lin-agent/previous-input)
    (define-key map (kbd "M-n")     #'lin-agent/next-input)
    ;; Actions
    (define-key map (kbd "C-c C-k") #'lin-agent/stop)
    (define-key map (kbd "C-c C-s") #'lin-agent/save-session)
    (define-key map (kbd "C-c C-r") #'lin-agent/rewind)
    (define-key map (kbd "C-c C-i") #'lin-agent/inspect-context)
    (define-key map (kbd "C-c C-w") #'lin-agent/copy-last-response)
    (define-key map (kbd "C-c C-a") #'lin-agent-insert-mention)
    (define-key map (kbd "C-g")     (lambda () (interactive)
                                     (when (fboundp 'lin-agent--stream-stop)
                                       (lin-agent--stream-stop))))
    (define-key map (kbd "q")       (lambda () (interactive)
                                     (if (and lin-agent--input-marker
                                              (marker-position lin-agent--input-marker)
                                              (>= (point) lin-agent--input-marker))
                                         (self-insert-command 1)
                                       (quit-window))))
    map)
  "Keymap for `lin-agent-mode'.")

;; ==================== Major Mode ====================

(define-derived-mode lin-agent-mode nil "Claude-Agent"
  "Major mode for Claude Agent interaction buffer.
Does NOT derive from special-mode because special-mode:
  1) sets buffer-read-only=t  → blocks all writing
  2) remaps self-insert-command → undefined  → blocks all typing
  3) binds SPC/DEL/g/h/q etc.  → steals common keys

Instead we use text properties for read-only regions and keep
the input area fully writable.

In the input area (after ❯):
  RET         Submit message
  M-p / M-n   Navigate input history
  C-c C-c     Send via minibuffer (alternative)
  C-c C-k     Stop session
  C-g         Stop streaming

\\{lin-agent-mode-map}"
  :group 'lin-agent
  (setq-local font-lock-defaults '(lin-agent-mode-font-lock-keywords t))
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 2)
  (setq-local buffer-read-only nil)
  (make-local-variable 'lin-agent--input-marker)
  (make-local-variable 'lin-agent--prompt-start-marker)
  (font-lock-mode 1))

;; Evil integration: use Emacs state so our keymap and input area work
(with-eval-after-load 'evil
  (evil-set-initial-state 'lin-agent-mode 'emacs))

;; ==================== Project Config ====================

(defvar lin-agent-project-config-names
  '(".lin-agent.md" ".claude.md" "CLAUDE.md"))

(defun lin-agent--load-project-config ()
  (let ((root (or (and (fboundp 'projectile-project-root)
                       (ignore-errors (projectile-project-root)))
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
                           (lambda (block) (or (alist-get 'text block) ""))
                           content ""))
                         ((listp content)
                          (mapconcat
                           (lambda (block) (or (alist-get 'text block) ""))
                           content "")))))
              (when (and text (not (string-empty-p text)))
                (kill-new text)
                (message "Copied %d chars." (length text))))))))))

;; ==================== Session Rename ====================

(defvar lin-agent--session-name nil)

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
