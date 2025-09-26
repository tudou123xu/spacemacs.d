;;; ai-assistant.el --- AI 助手统一模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 包定义 ====================
(defconst ai-assistant-packages
  '(aider ellama which-function cl-lib json)
  "AI 助手相关包列表")

;; ==================== 配置 ====================
(defvar ai-assistant/current-provider "deepseek"
  "当前使用的 AI 提供商")

(defun ai-assistant/init-ai-assistant ()
  "初始化 AI 助手模块"
  (ai-assistant/setup-aider)
  (ai-assistant/setup-ellama)
  (ai-assistant/setup-keybindings))

(defun ai-assistant/setup-aider ()
  "配置 Aider"
  (when (package-installed-p 'aider)
    (setq aider-model "gpt-4"
          aider-max-tokens 2000
          aider-ask-confirm t
          aider-deepseek-api-key (getenv "DEEPSEEK_API_KEY")
          aider-deepseek-api-base "https://api.deepseek.com/v1"
          aider-deepseek-model "deepseek-chat"
          aider-deepseek-max-tokens 4000
          aider-deepseek-temperature 0.7)
    (add-hook 'aider-mode-hook #'ai-assistant/check-environment)))

(defun ai-assistant/setup-ellama ()
  "配置 Ellama"
  (when (package-installed-p 'ellama)
    (setq ellama-provider 'ollama
          ellama-model "llama3:8b"
          ellama-api-base "http://localhost:11434")))

;; ==================== 功能函数 ====================
(defun ai-assistant/switch-provider (provider)
  "切换 AI 提供商"
  (interactive (list (completing-read "选择 AI 提供商: " '("deepseek" "openai" "local"))))
  (setq ai-assistant/current-provider provider)
  (message "已切换到 %s 提供商" provider))

(defun ai-assistant/check-environment ()
  "检查 AI 环境配置"
  (message "=== AI 环境检查 ===")
  (message "OpenAI: %s" (if (getenv "OPENAI_API_KEY") "已配置" "未配置"))
  (message "DeepSeek: %s" (if (getenv "DEEPSEEK_API_KEY") "已配置" "未配置"))
  (message "当前提供商: %s" ai-assistant/current-provider))

(defun ai-assistant/start-session (&optional project-path)
  "启动 AI 助手会话"
  (interactive)
  (let* ((default-directory (or project-path 
                                (if (fboundp 'projectile-project-root)
                                    (projectile-project-root)
                                  default-directory)))
         (session-name (format "ai-assistant-%s" (file-name-nondirectory (directory-file-name default-directory))))
         (buffer-name (format "*%s*" session-name)))
    (unless (get-buffer buffer-name)
      (let ((ai-buffer (get-buffer-create buffer-name)))
        (with-current-buffer ai-buffer
          (erase-buffer)
          (insert "=== AI 助手会话 ===\n\n")
          (insert "会话名称: " session-name "\n")
          (insert "项目路径: " default-directory "\n")
          (insert "启动时间: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
          (insert "AI 助手功能:\n")
          (insert "- 代码审查: M-x ai-assistant/ai-code-review\n")
          (insert "- 代码优化: M-x ai-assistant/ai-optimize-code\n")
          (insert "- 代码解释: M-x ai-assistant/explain-region\n")
          (insert "- 函数审查: M-x ai-assistant/review-current-function\n")
          (insert "- 切换提供商: M-x ai-assistant/switch-provider\n")
          (insert "- 快速菜单: M-x ai-assistant/quick-menu\n\n")
          (insert "提示: 选择代码后使用相应的 AI 功能\n")
          (setq buffer-read-only t)
          (goto-char (point-min)))
        (message "AI 助手会话已启动: %s" session-name)))
    (switch-to-buffer buffer-name)))

(defun ai-assistant/ai-code-review (file-path)
  "使用 AI 进行代码审查"
  (interactive "f选择要审查的文件: ")
  (when (file-exists-p file-path)
    (let ((buffer-name "*AI Code Review*"))
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert "=== AI 代码审查 ===\n\n")
        (insert "文件: " file-path "\n")
        (insert "时间: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
        (insert "正在使用 AI 分析代码...\n")
        (pop-to-buffer buffer-name))
      (message "AI 代码审查已启动"))))

(defun ai-assistant/ai-optimize-code (start end)
  "使用 AI 优化选中的代码"
  (interactive "r")
  (when (region-active-p)
    (let ((buffer-name "*AI Code Optimization*"))
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert "=== AI 代码优化 ===\n\n")
        (insert "时间: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
        (insert "正在使用 AI 优化代码...\n")
        (pop-to-buffer buffer-name))
      (message "AI 代码优化已启动"))))

(defun ai-assistant/explain-region (start end)
  "解释选中的代码区域"
  (interactive "r")
  (when (region-active-p)
    (let ((code (buffer-substring-no-properties start end))
          (buffer-name "*AI Code Explanation*"))
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert "=== AI 代码解释 ===\n\n")
        (insert "代码:\n```\n" code "\n```\n\n")
        (insert "正在使用 AI 解释代码...\n")
        (pop-to-buffer buffer-name))
      (message "AI 代码解释已启动"))))

(defun ai-assistant/review-current-function ()
  "审查当前函数"
  (interactive)
  (let ((func-name (which-function)))
    (if func-name
        (let ((buffer-name "*AI Function Review*"))
          (with-current-buffer (get-buffer-create buffer-name)
            (erase-buffer)
            (insert "=== AI 函数审查 ===\n\n")
            (insert "函数: " func-name "\n")
            (insert "时间: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
            (insert "正在使用 AI 审查函数...\n")
            (pop-to-buffer buffer-name))
          (message "AI 函数审查已启动"))
      (message "无法识别当前函数"))))

(defun ai-assistant/quick-menu ()
  "显示 AI 助手快速访问菜单"
  (interactive)
  (let ((menu-options '("代码审查" "代码优化" "代码解释" "切换模型"))
        (choice (completing-read "选择 AI 功能: " menu-options)))
    (pcase choice
      ("代码审查" (ai-assistant/ai-code-review (buffer-file-name)))
      ("代码优化" (if (region-active-p)
                      (ai-assistant/ai-optimize-code (region-beginning) (region-end))
                    (message "请先选择要优化的代码区域")))
      ("代码解释" (if (region-active-p)
                      (ai-assistant/explain-region (region-beginning) (region-end))
                    (message "请先选择要解释的代码区域")))
      ("切换模型" (ai-assistant/switch-provider nil)))))

(defun ai-assistant/simple-start ()
  "简单启动 AI 助手（不依赖外部包）"
  (interactive)
  (let ((buffer-name "*AI Assistant*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert "=== AI 助手 ===\n\n")
      (insert "欢迎使用 AI 助手！\n\n")
      (insert "可用功能:\n")
      (insert "1. 代码审查: M-x ai-assistant/ai-code-review\n")
      (insert "2. 代码优化: M-x ai-assistant/ai-optimize-code\n")
      (insert "3. 代码解释: M-x ai-assistant/explain-region\n")
      (insert "4. 函数审查: M-x ai-assistant/review-current-function\n")
      (insert "5. 切换提供商: M-x ai-assistant/switch-provider\n")
      (insert "6. 快速菜单: M-x ai-assistant/quick-menu\n\n")
      (insert "使用说明:\n")
      (insert "- 选择代码后使用相应的 AI 功能\n")
      (insert "- 所有功能都会在独立缓冲区中显示结果\n")
      (insert "- 支持 DeepSeek、OpenAI 和本地模型\n\n")
      (insert "当前提供商: " ai-assistant/current-provider "\n")
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (switch-to-buffer buffer-name)
    (message "AI 助手已启动")))

;; ==================== 快捷键配置 ====================
(defun ai-assistant/setup-keybindings ()
  "配置 AI 助手快捷键"
  (when (fboundp 'spacemacs/set-leader-keys)
    (spacemacs/set-leader-keys
      "oas"  #'ai-assistant/start-session
      "oaa"  #'ai-assistant/simple-start
      "oad"  #'ai-assistant/ai-code-review
      "aao"  #'ai-assistant/ai-optimize-code
      "aae"  #'ai-assistant/explain-region
      "aar"  #'ai-assistant/review-current-function
      "aas"  #'ai-assistant/switch-provider
      "aam"  #'ai-assistant/quick-menu)
    
    (when (fboundp 'spacemacs/set-leader-keys-for-major-mode)
      (spacemacs/set-leader-keys-for-major-mode 'prog-mode
        "ad"   #'ai-assistant/ai-code-review
        "ao"   #'ai-assistant/ai-optimize-code
        "ae"   #'ai-assistant/explain-region
        "ar"   #'ai-assistant/review-current-function)))
  
  (global-set-key (kbd "C-c a s") #'ai-assistant/start-session)
  (global-set-key (kbd "C-c a a") #'ai-assistant/simple-start)
  (global-set-key (kbd "C-c a d") #'ai-assistant/ai-code-review)
  (global-set-key (kbd "C-c a o") #'ai-assistant/ai-optimize-code)
  (global-set-key (kbd "C-c a e") #'ai-assistant/explain-region)
  (global-set-key (kbd "C-c a r") #'ai-assistant/review-current-function)
  (global-set-key (kbd "C-c a p") #'ai-assistant/switch-provider)
  (global-set-key (kbd "C-c a m") #'ai-assistant/quick-menu)
  
  (with-eval-after-load 'evil
    (when (fboundp 'evil-define-key)
      (evil-define-key 'visual 'global
        (kbd "SPC o a o") #'ai-assistant/ai-optimize-code
        (kbd "SPC o a e") #'ai-assistant/explain-region))))

;; ==================== 初始化 ====================
(ai-assistant/init-ai-assistant)

(provide 'ai-assistant)
;;; ai-assistant.el ends here
