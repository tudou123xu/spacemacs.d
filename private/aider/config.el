;; ~/.spacemacs.d/private/aider/config.el
(use-package aider
  :ensure t
  :config
  ;; ==================== 基础配置 ====================
  (setq aider-model "gpt-4"
        aider-max-tokens 2000
        aider-ask-confirm t)
  
  ;; ==================== DeepSeek 接入配置 ====================
  ;; DeepSeek API 配置
  (setq aider-deepseek-api-key (getenv "DEEPSEEK_API_KEY")
        aider-deepseek-api-base "https://api.deepseek.com/v1"
        aider-deepseek-model "deepseek-chat"
        aider-deepseek-max-tokens 4000
        aider-deepseek-temperature 0.7)
  
  ;; DeepSeek 模型选择
  (setq aider-deepseek-available-models
        '("deepseek-chat"           ; 通用对话模型
          "deepseek-coder"          ; 代码专用模型
          "deepseek-chat-instruct"  ; 指令跟随模型
          "deepseek-vision"         ; 多模态模型
          "deepseek-embedding"))    ; 嵌入模型
  
  ;; ==================== 多模型支持 ====================
  (defvar aider-current-provider "openai"
    "当前使用的 AI 提供商: openai, deepseek, local")
  
  (defvar aider-providers
    '((openai . (:model "gpt-4"
                  :max-tokens 2000
                  :temperature 0.7))
      (deepseek . (:model "deepseek-chat"
                   :max-tokens 4000
                   :temperature 0.7))
      (local . (:model "llama3:8b"
                :max-tokens 2000
                :temperature 0.7)))
    "支持的 AI 提供商配置")
  
  ;; ==================== 智能模型选择 ====================
  (defun aider/select-best-model (task-type)
    "根据任务类型选择最佳模型"
    (cond
     ((string-match-p "代码\\|编程\\|debug" task-type)
      (if (and aider-deepseek-api-key (member "deepseek-coder" aider-deepseek-available-models))
          "deepseek-coder"
        "gpt-4"))
     ((string-match-p "解释\\|分析\\|理解" task-type)
      (if (and aider-deepseek-api-key (member "deepseek-chat" aider-deepseek-available-models))
          "deepseek-chat"
        "gpt-4"))
     (t "gpt-4")))
  
  ;; ==================== 模型切换功能 ====================
  (defun aider/switch-provider (provider)
    "切换 AI 提供商"
    (interactive (list (completing-read "选择 AI 提供商: "
                                       '("openai" "deepseek" "local"))))
    (setq aider-current-provider (intern provider))
    (let ((config (alist-get (intern provider) aider-providers)))
      (setq aider-model (plist-get config :model)
            aider-max-tokens (plist-get config :max-tokens))
      (message "已切换到 %s 提供商，模型: %s" provider aider-model)))
  
  ;; ==================== DeepSeek 专用函数 ====================
  (defun aider/deepseek-chat (prompt)
    "使用 DeepSeek 进行对话"
    (when aider-deepseek-api-key
      (let ((url-request-method "POST")
            (url-request-extra-headers
             `(("Authorization" . ,(concat "Bearer " aider-deepseek-api-key))
               ("Content-Type" . "application/json")))
            (url-request-data
             (json-encode
              `((model . ,aider-deepseek-model)
                (messages . [((role . "user") (content . ,prompt))])
                (max_tokens . ,aider-deepseek-max-tokens)
                (temperature . ,aider-deepseek-temperature))))))
        ;; 这里应该调用实际的 API，简化处理
        (message "DeepSeek 对话: %s" prompt))))
  
  ;; ==================== 环境检查 ====================
  (defun aider/check-environment ()
    "检查 AI 环境配置"
    (message "=== AI 环境检查 ===")
    (message "OpenAI: %s" (if (getenv "OPENAI_API_KEY") "已配置" "未配置"))
    (message "DeepSeek: %s" (if aider-deepseek-api-key "已配置" "未配置"))
    (message "当前提供商: %s" aider-current-provider)
    (message "当前模型: %s" aider-model))
  
  ;; ==================== 初始化 ====================
  (add-hook 'aider-mode-hook #'aider/check-environment)
  
  ;; 检查 DeepSeek 配置
  (when aider-deepseek-api-key
    (message "DeepSeek API 已配置，模型: %s" aider-deepseek-model))
  
  (message "Aider 配置已加载，支持 OpenAI、DeepSeek 和本地模型"))
