;;; funcs.el --- lin-ai layer functions file for Spacemacs. -*- lexical-binding: t; -*-
;; Author: xuzhifeng

;; ==================== Secure Key Management ====================

(defun lin-ai/get-api-key (host user)
  "Retrieve API key from auth-source or fallback to environment variable.
Supports .authinfo, .authinfo.gpg, and environment variables."
  (let ((auth (car (auth-source-search :host host :user user))))
    (if auth
        (funcall (plist-get auth :secret))
      (getenv (format "%s_API_KEY" (upcase user))))))

;; ==================== Provider Registry ====================

(defvar lin-ai--keys-cache nil
  "Cached API keys to avoid repeated auth-source lookups.")

(defun lin-ai/load-keys ()
  "Load all API keys once and cache them."
  (unless lin-ai--keys-cache
    (setq lin-ai--keys-cache
          `((deepseek    . ,(lin-ai/get-api-key "api.deepseek.com" "deepseek"))
            (siliconflow . ,(lin-ai/get-api-key "api.siliconflow.cn" "siliconflow"))
            (claude      . ,(or (lin-ai/get-api-key "api.anthropic.com" "claude")
                                (getenv "ANTHROPIC_API_KEY")))
            (openai      . ,(or (lin-ai/get-api-key "api.openai.com" "openai")
                                (getenv "OPENAI_API_KEY"))))))
  lin-ai--keys-cache)

(defun lin-ai/get-key (provider)
  "Get cached API key for PROVIDER (symbol)."
  (alist-get provider (lin-ai/load-keys)))

(defvar lin-ai-providers nil
  "Supported AI providers registry. Populated by `lin-ai/load-keys'.")

(defun lin-ai/init-providers ()
  "Initialize provider registry with available keys."
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
                `((siliconflow . (:model "deepseek-ai/DeepSeek-V2-Chat"
                                  :api-base "https://api.siliconflow.cn/v1"
                                  :api-key ,(alist-get 'siliconflow keys)))))
            ,@(when (alist-get 'openai keys)
                `((openai . (:model "gpt-4o"
                             :api-base "https://api.openai.com/v1"
                             :api-key ,(alist-get 'openai keys)))))
            (local . (:model "llama3:8b"
                      :api-base "http://localhost:11434"))))))

(defun lin-ai/available-providers ()
  "Return list of provider names that have valid API keys."
  (unless lin-ai-providers (lin-ai/init-providers))
  (mapcar #'car lin-ai-providers))

(defun lin-ai/get-provider (name)
  "Get provider config plist by NAME (symbol)."
  (unless lin-ai-providers (lin-ai/init-providers))
  (alist-get name lin-ai-providers))

;; ==================== Model Selection ====================

(defun lin-ai/select-best-model (task-type)
  "Select the best model based on TASK-TYPE and availability."
  (lin-ai/load-keys)
  (let ((claude-key (alist-get 'claude lin-ai--keys-cache))
        (deepseek-key (alist-get 'deepseek lin-ai--keys-cache))
        (siliconflow-key (alist-get 'siliconflow lin-ai--keys-cache)))
    (cond
     ((string-match-p "agent\\|complex\\|architect" task-type)
      (cond (claude-key '(claude . "claude-sonnet-4-20250514"))
            (deepseek-key '(deepseek . "deepseek-chat"))
            (t '(local . "llama3:8b"))))
     ((string-match-p "code\\|programming\\|debug" task-type)
      (cond (claude-key '(claude . "claude-sonnet-4-20250514"))
            (deepseek-key '(deepseek . "deepseek-coder"))
            (siliconflow-key '(siliconflow . "deepseek-ai/DeepSeek-V2-Coder"))
            (t '(local . "llama3:8b"))))
     ((string-match-p "explain\\|analyze\\|chat" task-type)
      (cond (claude-key '(claude . "claude-sonnet-4-20250514"))
            (deepseek-key '(deepseek . "deepseek-chat"))
            (siliconflow-key '(siliconflow . "deepseek-ai/DeepSeek-V2-Chat"))
            (t '(local . "llama3:8b"))))
     (t
      (cond (claude-key '(claude . "claude-sonnet-4-20250514"))
            (deepseek-key '(deepseek . "deepseek-chat"))
            (t '(local . "llama3:8b")))))))

;; ==================== Aider Configuration ====================

(defun lin-ai/configure-aider ()
  "Configure Aider with available model backends."
  (setq aider-ask-confirm t)
  (lin-ai/load-keys)
  (let ((claude-key (alist-get 'claude lin-ai--keys-cache))
        (deepseek-key (alist-get 'deepseek lin-ai--keys-cache))
        (siliconflow-key (alist-get 'siliconflow lin-ai--keys-cache)))
    (cond
     (claude-key
      (setq aider-model "claude-sonnet-4-20250514"
            aider-api-base "https://api.anthropic.com"
            aider-api-key claude-key))
     (deepseek-key
      (setq aider-model "deepseek-chat"
            aider-api-base "https://api.deepseek.com/v1"
            aider-api-key deepseek-key))
     (siliconflow-key
      (setq aider-model "deepseek-ai/DeepSeek-V2-Chat"
            aider-api-base "https://api.siliconflow.cn/v1"
            aider-api-key siliconflow-key)))))

;; ==================== Router ====================

(defun lin-ai/router (task-type prompt)
  "Route AI task to appropriate backend."
  (interactive "sTask Type (code/chat/agent): \nsPrompt: ")
  (cond
   ((string= task-type "agent")
    (if (fboundp 'lin-agent/send-message)
        (lin-agent/send-message prompt)
      (message "lin-agent layer not loaded")))
   ((string= task-type "code")
    (if (fboundp 'ellama-code-complete)
        (ellama-code-complete prompt)
      (message "Ellama not available for code completion")))
   (t
    (if (fboundp 'aider-ask)
        (aider-ask prompt)
      (message "Aider not available for chat")))))

;;; funcs.el ends here
