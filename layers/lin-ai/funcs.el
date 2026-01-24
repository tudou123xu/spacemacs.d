;;; funcs.el --- lin-ai layer functions file for Spacemacs. -*- lexical-binding: t; -*-

(defun lin-ai/get-api-key (host user)
  "Retrieve API key from auth-source or fallback to environment variable."
  (let ((auth (car (auth-source-search :host host :user user))))
    (if auth
        (funcall (plist-get auth :secret))
      (getenv (format "%s_API_KEY" (upcase user))))))

(defun lin-ai/configure-aider ()
  "Configure Aider with DeepSeek, SiliconFlow, and local models."
  (setq aider-ask-confirm t)
  
  ;; Retrieve Keys securely
  (setq lin-ai-deepseek-key (lin-ai/get-api-key "api.deepseek.com" "deepseek"))
  (setq lin-ai-siliconflow-key (lin-ai/get-api-key "api.siliconflow.cn" "siliconflow"))

  ;; Default to DeepSeek if available, otherwise SiliconFlow
  (cond
   (lin-ai-deepseek-key
    (setq aider-model "deepseek-chat"
          aider-api-base "https://api.deepseek.com/v1"
          aider-api-key lin-ai-deepseek-key))
   (lin-ai-siliconflow-key
    (setq aider-model "deepseek-ai/DeepSeek-V2-Chat"
          aider-api-base "https://api.siliconflow.cn/v1"
          aider-api-key lin-ai-siliconflow-key)))

  (defvar lin-ai-providers
    `((deepseek . (:model "deepseek-chat" 
                   :api-base "https://api.deepseek.com/v1"
                   :api-key ,lin-ai-deepseek-key))
      (siliconflow . (:model "deepseek-ai/DeepSeek-V2-Chat"
                      :api-base "https://api.siliconflow.cn/v1"
                      :api-key ,lin-ai-siliconflow-key))
      (local . (:model "llama3:8b" 
                :api-base "http://localhost:11434")))
    "Supported AI providers registry.")

  (defun lin-ai/select-best-model (task-type)
    "Select the best model based on TASK-TYPE and availability."
    (cond
     ((string-match-p "code\\|programming\\|debug" task-type)
      (cond (lin-ai-deepseek-key "deepseek-coder")
            (lin-ai-siliconflow-key "deepseek-ai/DeepSeek-V2-Coder")
            (t "gpt-4")))
     ((string-match-p "explain\\|analyze" task-type)
      (cond (lin-ai-deepseek-key "deepseek-chat")
            (lin-ai-siliconflow-key "deepseek-ai/DeepSeek-V2-Chat")
            (t "gpt-4")))
     (t "gpt-4"))))

(defun lin-ai/router (task-type prompt)
  "Route AI task to appropriate backend."
  (interactive "sTask Type (code/chat): \nsPrompt: ")
  (if (string= task-type "code")
      (if (fboundp 'ellama-code-complete)
          (ellama-code-complete prompt)
        (message "Ellama not available for code completion"))
    (if (fboundp 'aider-ask)
        (aider-ask prompt)
      (message "Aider not available for chat"))))

;;; funcs.el ends here
