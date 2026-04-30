;;; funcs.el --- lin-ai layer functions for Spacemacs. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Core AI functions: provider management, gptel backends, IDE integration.

(require 'cl-lib)

;; ==================== Secure Key Management ====================

(defun lin-ai/get-api-key (host user)
  "Retrieve API key from auth-source or environment variable."
  (let ((auth (car (auth-source-search :host host :user user))))
    (if auth
        (funcall (plist-get auth :secret))
      (getenv (format "%s_API_KEY" (upcase user))))))

(defvar lin-ai--keys-cache nil)

(defun lin-ai/load-keys ()
  "Load all API keys and cache them."
  (unless lin-ai--keys-cache
    (setq lin-ai--keys-cache
          `((deepseek    . ,(lin-ai/get-api-key "api.deepseek.com" "deepseek"))
            (siliconflow . ,(lin-ai/get-api-key "api.siliconflow.cn" "siliconflow"))
            (claude      . ,(or (lin-ai/get-api-key "api.anthropic.com" "claude")
                                (getenv "ANTHROPIC_API_KEY")))
            (openai      . ,(or (lin-ai/get-api-key "api.openai.com" "openai")
                                (getenv "OPENAI_API_KEY"))))))
  lin-ai--keys-cache)

(defvar lin-ai--runtime-keys nil)

(defun lin-ai/set-key (provider key)
  "Set API KEY for PROVIDER at runtime."
  (interactive
   (list (intern (completing-read "Provider: " '("claude" "deepseek" "openai" "siliconflow") nil t))
         (read-passwd "API Key: ")))
  (setf (alist-get provider lin-ai--runtime-keys) key)
  (setq lin-ai--keys-cache nil)
  (message "%s API key set." provider))

(defun lin-ai/get-key (provider)
  "Get API key for PROVIDER. Priority: runtime > builtin > authinfo > env."
  (or (alist-get provider lin-ai--runtime-keys)
      (and (boundp 'lin-agent-builtin-keys)
           (alist-get provider lin-agent-builtin-keys))
      (alist-get provider (lin-ai/load-keys))))

(defvar lin-ai-providers nil)

(defun lin-ai/init-providers ()
  "Initialize provider registry."
  (lin-ai/load-keys)
  (setq lin-ai-providers
        (let ((keys lin-ai--keys-cache))
          `(,@(when (alist-get 'claude keys)
                `((claude . (:model "claude-sonnet-4-20250514"
                             :api-base "https://api.anthropic.com"
                             :api-key ,(alist-get 'claude keys)))))
            ,@(when (alist-get 'deepseek keys)
                `((deepseek . (:model "deepseek-chat"
                               :api-base "https://api.deepseek.com/v1"
                               :api-key ,(alist-get 'deepseek keys)))))
            ,@(when (alist-get 'siliconflow keys)
                `((siliconflow . (:model "deepseek-ai/DeepSeek-V3"
                                  :api-base "https://api.siliconflow.cn/v1"
                                  :api-key ,(alist-get 'siliconflow keys)))))
            ,@(when (alist-get 'openai keys)
                `((openai . (:model "gpt-4o"
                             :api-base "https://api.openai.com/v1"
                             :api-key ,(alist-get 'openai keys)))))
            (local . (:model "llama3:8b"
                      :api-base "http://localhost:11434"))))))

(defun lin-ai/available-providers ()
  (unless lin-ai-providers (lin-ai/init-providers))
  (mapcar #'car lin-ai-providers))

(defun lin-ai/get-provider (name)
  (unless lin-ai-providers (lin-ai/init-providers))
  (alist-get name lin-ai-providers))

;; ==================== gptel Backend Setup ====================

(defvar lin-ai--gptel-siliconflow nil)
(defvar lin-ai--gptel-deepseek nil)

(defun lin-ai/setup-gptel-backends ()
  "Register SiliconFlow and DeepSeek as gptel backends."
  (require 'gptel)
  (require 'gptel-openai)
  (let ((sf-key (lin-ai/get-key 'siliconflow))
        (ds-key (lin-ai/get-key 'deepseek)))
    (when sf-key
      (setq lin-ai--gptel-siliconflow
            (gptel-make-openai "SiliconFlow"
              :host "api.siliconflow.cn"
              :endpoint "/v1/chat/completions"
              :stream t
              :key sf-key
              :models '(deepseek-ai/DeepSeek-V3
                        deepseek-ai/DeepSeek-R1
                        Qwen/Qwen2.5-Coder-32B-Instruct
                        Pro/deepseek-ai/DeepSeek-V3))))
    (when ds-key
      (setq lin-ai--gptel-deepseek
            (gptel-make-openai "DeepSeek"
              :host "api.deepseek.com"
              :endpoint "/v1/chat/completions"
              :stream t
              :key ds-key
              :models '(deepseek-chat
                        deepseek-coder
                        deepseek-reasoner))))
    (setq gptel-backend (or lin-ai--gptel-siliconflow
                            lin-ai--gptel-deepseek
                            gptel--openai)
          gptel-model (if lin-ai--gptel-siliconflow
                         'deepseek-ai/DeepSeek-V3
                       'deepseek-chat))))

;; ==================== Cursor-Style IDE Functions ====================

(defun lin-ai/chat ()
  "Open AI chat panel (like Cursor Cmd+L).
If region is active, include selected code as context."
  (interactive)
  (require 'gptel)
  (unless gptel-backend (lin-ai/setup-gptel-backends))
  (if (use-region-p)
      (let ((code (buffer-substring-no-properties (region-beginning) (region-end)))
            (mode-name (symbol-name major-mode))
            (file (or (buffer-file-name) (buffer-name))))
        (gptel (format "*AI Chat: %s*" (file-name-nondirectory file)))
        (goto-char (point-max))
        (insert (format "\nContext from `%s` (%s):\n```\n%s\n```\n\n" file mode-name code)))
    (gptel "*AI Chat*")))

(defun lin-ai--gptel-request-active-in-buffer-p (&optional buffer)
  "Non-nil if gptel has an in-flight request for BUFFER (default: current).
Matches the lookup used by `gptel-abort' (gptel-request.el)."
  (and (featurep 'gptel)
       (fboundp 'gptel-fsm-info)
       (boundp 'gptel--request-alist)
       (let ((buf (or buffer (current-buffer))))
         (cl-find-if
          (lambda (entry)
            (ignore-errors
              (eq (plist-get (gptel-fsm-info (cadr entry)) :buffer) buf)))
          gptel--request-alist))))

(defun lin-ai/cursor-cmd-k ()
  "Like Cursor Cmd+K: abort gptel stream, or close gptel chat, or start inline edit."
  (interactive)
  (require 'gptel)
  (unless gptel-backend (lin-ai/setup-gptel-backends))
  (cond
   ((lin-ai--gptel-request-active-in-buffer-p)
    (when (fboundp 'gptel-abort)
      (gptel-abort (current-buffer))))
   ((eq major-mode 'gptel-mode)
    (quit-window))
   (t
    (call-interactively #'lin-ai/inline-edit))))

(defun lin-ai/inline-edit ()
  "AI inline edit: rewrite selected code or generate at point (like Cursor Cmd+K).
With region: rewrite/transform the selection.
Without region: generate code at cursor position."
  (interactive)
  (require 'gptel)
  (unless gptel-backend (lin-ai/setup-gptel-backends))
  (if (use-region-p)
      (gptel-rewrite)
    (let ((prompt (read-string "Generate code: ")))
      (gptel-request prompt
        :stream t
        :in-place t
        :position (point)))))

(defun lin-ai/fix-error ()
  "Send the current flycheck/flymake error to AI for a fix (like Cursor's auto-fix).
Selects the error region and asks gptel to fix it."
  (interactive)
  (require 'gptel)
  (unless gptel-backend (lin-ai/setup-gptel-backends))
  (let* ((errs (or (and (bound-and-true-p flycheck-mode)
                        (flycheck-overlay-errors-at (point)))
                   (and (bound-and-true-p flymake-mode)
                        (flymake-diagnostics (point)))))
         (err-msg (cond
                   ((and errs (bound-and-true-p flycheck-mode))
                    (flycheck-error-message (car errs)))
                   ((and errs (bound-and-true-p flymake-mode))
                    (flymake-diagnostic-text (car errs)))
                   (t nil))))
    (if err-msg
        (let* ((fn-bounds (or (bounds-of-thing-at-point 'defun)
                              (cons (line-beginning-position) (line-end-position))))
               (code (buffer-substring-no-properties (car fn-bounds) (cdr fn-bounds))))
          (set-mark (car fn-bounds))
          (goto-char (cdr fn-bounds))
          (gptel-request
           (format "Fix this error: %s\n\nCode:\n```\n%s\n```\n\nReturn ONLY the corrected code, no explanation."
                   err-msg code)
           :stream t
           :in-place t
           :position (car fn-bounds)))
      (message "No error at point. Move cursor to an error marker first."))))

(defun lin-ai/explain-code ()
  "Explain the selected code or function at point."
  (interactive)
  (require 'gptel)
  (unless gptel-backend (lin-ai/setup-gptel-backends))
  (let* ((code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'defun t)))
         (lang (symbol-name major-mode)))
    (if code
        (let ((buf (gptel (format "*AI Explain*"))))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert (format "\n请解释这段代码 (%s):\n```\n%s\n```\n" lang code))
            (gptel-send)))
      (message "No code at point or selected region."))))

(defun lin-ai/generate-docstring ()
  "Generate a docstring for the function at point."
  (interactive)
  (require 'gptel)
  (unless gptel-backend (lin-ai/setup-gptel-backends))
  (let ((fn-code (thing-at-point 'defun t))
        (lang (symbol-name major-mode)))
    (if fn-code
        (let ((gptel-directives '((default . "Generate only the docstring/documentation comment. Follow the language's doc conventions. Output ONLY the docstring, no other code."))))
          (gptel-request
           (format "Generate a docstring for this %s function:\n```\n%s\n```" lang fn-code)
           :stream t
           :in-place t
           :position (save-excursion
                       (beginning-of-defun)
                       (forward-line 1)
                       (point))))
      (message "No function at point."))))

(defun lin-ai/refactor ()
  "Refactor the selected code or function at point."
  (interactive)
  (require 'gptel)
  (unless gptel-backend (lin-ai/setup-gptel-backends))
  (if (use-region-p)
      (let ((gptel-directives '((default . "Refactor this code: improve readability, reduce complexity, follow best practices. Return ONLY the refactored code."))))
        (gptel-rewrite))
    (message "Select a region to refactor.")))

(defun lin-ai/send-buffer-or-region ()
  "Send buffer or region to AI chat with streaming response.
Like Cursor's inline chat: select code, ask a question about it."
  (interactive)
  (require 'gptel)
  (unless gptel-backend (lin-ai/setup-gptel-backends))
  (gptel-send))

(defun lin-ai/quick-ask (question)
  "Ask a quick question about the current file/project context."
  (interactive "sAsk AI: ")
  (require 'gptel)
  (unless gptel-backend (lin-ai/setup-gptel-backends))
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (lang (symbol-name major-mode))
         (context (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (buffer-substring-no-properties
                     (max (point-min) (- (point) 2000))
                     (min (point-max) (+ (point) 2000)))))
         (prompt (format "File: %s (%s)\nContext:\n```\n%s\n```\n\nQuestion: %s"
                         file lang context question)))
    (gptel-request prompt
      :callback (lambda (response _info)
                  (when response
                    (with-current-buffer (get-buffer-create "*AI Answer*")
                      (erase-buffer)
                      (org-mode)
                      (insert "* AI Answer\n\n" response)
                      (goto-char (point-min))
                      (display-buffer (current-buffer))))))))

(defun lin-ai/switch-backend ()
  "Switch gptel backend interactively."
  (interactive)
  (require 'gptel)
  (unless gptel-backend (lin-ai/setup-gptel-backends))
  (call-interactively #'gptel-menu))

;; ==================== Aider Integration ====================

(defun lin-ai/configure-aider ()
  "Configure Aider with available model backends."
  (when (fboundp 'aider-ask)
    (setq aider-ask-confirm t)
    (lin-ai/load-keys)
    (let ((sf-key (lin-ai/get-key 'siliconflow))
          (ds-key (lin-ai/get-key 'deepseek)))
      (cond
       (sf-key
        (setq aider-args `("--model" "openai/deepseek-ai/DeepSeek-V3"
                           "--api-key" ,(format "openai=%s" sf-key)
                           "--api-base" "https://api.siliconflow.cn/v1")))
       (ds-key
        (setq aider-args `("--model" "deepseek/deepseek-chat"
                           "--api-key" ,(format "deepseek=%s" ds-key))))))))

;; ==================== Cursor-style @mentions (gptel) ====================

(defun lin-ai--gptel-expand-mentions (fsm)
  "gptel `gptel-prompt-transform-functions' hook: expand @file / @folder."
  (when (and fsm (require 'lin-agent-mentions nil t) (fboundp 'gptel-fsm-info))
    (let* ((info (gptel-fsm-info fsm))
           (prompt-buf (plist-get info :data))
           (src-buf (plist-get info :buffer))
           (ctx (when (buffer-live-p src-buf)
                  (list :default-directory
                        (buffer-local-value 'default-directory src-buf)
                        :buffer-file
                        (buffer-local-value 'buffer-file-name src-buf)))))
      (when (buffer-live-p prompt-buf)
        (with-current-buffer prompt-buf
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (erase-buffer)
            (insert (lin-agent-expand-mentions text ctx))))))))

;; ==================== Router ====================

(defun lin-ai/router (task-type prompt)
  "Route AI task to appropriate backend."
  (interactive "sTask (chat/code/agent/fix): \nsPrompt: ")
  (cond
   ((string= task-type "agent")
    (if (fboundp 'lin-agent/send-message)
        (lin-agent/send-message prompt)
      (message "lin-agent layer not loaded")))
   ((string= task-type "fix")
    (lin-ai/fix-error))
   ((string= task-type "code")
    (lin-ai/inline-edit))
   (t
    (lin-ai/chat))))

;;; funcs.el ends here
