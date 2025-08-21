;;; funcs.el --- Aider 功能函数 -*- lexical-binding: t; -*-

;; ==================== Aider 会话管理 ====================
(defvar aider-sessions-history '()
  "Aider 会话历史记录")

(defun aider/start-session (&optional project-path)
  "启动 Aider 会话"
  (interactive)
  (let* ((default-directory (or project-path (projectile-project-root) default-directory))
         (session-name (format "aider-%s" (file-name-nondirectory (directory-file-name default-directory))))
         (buffer-name (format "*%s*" session-name)))
    
    (unless (get-buffer buffer-name)
      (let ((aider-buffer (vterm buffer-name)))
        (with-current-buffer aider-buffer
          (vterm-send-string (format "aider %s" (or aider-extra-args "")))
          (vterm-send-return))
        (add-to-list 'aider-sessions-history session-name)
        (message "Aider 会话已启动: %s" session-name)))
    
    (switch-to-buffer buffer-name)))

(defun aider/add-current-file ()
  "将当前文件添加到 Aider 会话"
  (interactive)
  (when (and buffer-file-name (aider/get-active-session))
    (let ((file-path (file-relative-name buffer-file-name (projectile-project-root))))
      (aider/send-command (format "/add %s" file-path))
      (message "已添加文件到 Aider: %s" file-path))))

(defun aider/send-command (command)
  "向活动的 Aider 会话发送命令"
  (when-let ((session-buffer (aider/get-active-session)))
    (with-current-buffer session-buffer
      (vterm-send-string command)
      (vterm-send-return))))

(defun aider/get-active-session ()
  "获取活动的 Aider 会话缓冲区"
  (cl-find-if (lambda (buf)
                (and (buffer-live-p buf)
                     (string-match-p "aider-" (buffer-name buf))))
              (buffer-list)))

;; ==================== 代码审查功能 ====================
(defun aider/review-current-function ()
  "审查当前函数"
  (interactive)
  (when (aider/get-active-session)
    (let ((func-name (which-function)))
      (if func-name
          (aider/send-command (format "请审查函数 %s 的代码质量和潜在问题" func-name))
        (message "无法识别当前函数")))))

(defun aider/explain-region (start end)
  "解释选中的代码区域"
  (interactive "r")
  (when (and (aider/get-active-session) (region-active-p))
    (let ((code (buffer-substring-no-properties start end)))
      (aider/send-command (format "请解释这段代码:\n```\n%s\n```" code)))))

;; ==================== 自动提交功能 ====================
(defvar aider-auto-commit-enabled nil
  "是否启用自动提交功能")

(defun aider/toggle-auto-commit ()
  "切换自动提交功能"
  (interactive)
  (setq aider-auto-commit-enabled (not aider-auto-commit-enabled))
  (aider/send-command (if aider-auto-commit-enabled "/auto-commits" "/no-auto-commits"))
  (message "Aider 自动提交: %s" (if aider-auto-commit-enabled "已启用" "已禁用")))

;; ==================== 工具函数 ====================
(defun aider/show-help ()
  "显示 Aider 帮助信息"
  (interactive)
  (aider/send-command "/help"))

(defun aider/list-files ()
  "列出当前会话中的文件"
  (interactive)
  (aider/send-command "/ls"))

(defun aider/quit-session ()
  "退出当前 Aider 会话"
  (interactive)
  (when (aider/get-active-session)
    (aider/send-command "/quit")
    (message "Aider 会话已退出")))

;; ==================== DeepSeek 集成功能 ====================
(defun aider/deepseek-code-review (file-path)
  "使用 DeepSeek 进行代码审查"
  (interactive "f选择要审查的文件: ")
  (when (and aider-deepseek-api-key (file-exists-p file-path))
    (let* ((file-content (with-temp-buffer
                           (insert-file-contents file-path)
                           (buffer-string)))
           (prompt (format "请审查以下代码，指出潜在问题、改进建议和最佳实践:\n\n```%s\n%s\n```"
                          (file-name-extension file-path)
                          file-content))
           (buffer-name "*DeepSeek Code Review*"))
      
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert "=== DeepSeek 代码审查 ===\n\n")
        (insert "文件: " file-path "\n")
        (insert "时间: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
        (insert "正在使用 DeepSeek 分析代码...\n")
        (pop-to-buffer buffer-name))
      
      ;; 调用 DeepSeek API
      (aider/deepseek-chat prompt))))

(defun aider/deepseek-optimize-code (start end)
  "使用 DeepSeek 优化选中的代码"
  (interactive "r")
  (when (and aider-deepseek-api-key (region-active-p))
    (let* ((code (buffer-substring-no-properties start end))
           (prompt (format "请优化以下代码，提高性能、可读性和最佳实践:\n\n```\n%s\n```\n\n请提供优化后的代码和解释。" code))
           (buffer-name "*DeepSeek Code Optimization*"))
      
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert "=== DeepSeek 代码优化 ===\n\n")
        (insert "时间: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
        (insert "正在使用 DeepSeek 优化代码...\n")
        (pop-to-buffer buffer-name))
      
      ;; 调用 DeepSeek API
      (aider/deepseek-chat prompt))))

(defun aider/deepseek-generate-tests (file-path)
  "使用 DeepSeek 为文件生成测试"
  (interactive "f选择要生成测试的文件: ")
  (when (and aider-deepseek-api-key (file-exists-p file-path))
    (let* ((file-content (with-temp-buffer
                           (insert-file-contents file-path)
                           (buffer-string)))
           (prompt (format "请为以下代码生成全面的测试用例，包括单元测试、边界测试和错误处理:\n\n```%s\n%s\n```\n\n请提供测试代码和测试策略。" 
                          (file-name-extension file-path)
                          file-content))
           (buffer-name "*DeepSeek Test Generation*"))
      
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert "=== DeepSeek 测试生成 ===\n\n")
        (insert "文件: " file-path "\n")
        (insert "时间: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
        (insert "正在使用 DeepSeek 生成测试...\n")
        (pop-to-buffer buffer-name))
      
      ;; 调用 DeepSeek API
      (aider/deepseek-chat prompt))))

(defun aider/deepseek-documentation (file-path)
  "使用 DeepSeek 生成代码文档"
  (interactive "f选择要生成文档的文件: ")
  (when (and aider-deepseek-api-key (file-exists-p file-path))
    (let* ((file-content (with-temp-buffer
                           (insert-file-contents file-path)
                           (buffer-string)))
           (prompt (format "请为以下代码生成详细的文档，包括函数说明、参数描述、返回值说明和使用示例:\n\n```%s\n%s\n```" 
                          (file-name-extension file-path)
                          file-content))
           (buffer-name "*DeepSeek Documentation*"))
      
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert "=== DeepSeek 文档生成 ===\n\n")
        (insert "文件: " file-path "\n")
        (insert "时间: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
        (insert "正在使用 DeepSeek 生成文档...\n")
        (pop-to-buffer buffer-name))
      
      ;; 调用 DeepSeek API
      (aider/deepseek-chat prompt))))

;; ==================== 智能任务路由 ====================
(defun aider/smart-task-router (task-description)
  "智能路由任务到最适合的 AI 模型"
  (interactive "s描述你的任务: ")
  (let ((best-model (aider/select-best-model task-description)))
    (cond
     ((string= best-model "deepseek-coder")
      (message "推荐使用 DeepSeek Coder 模型处理代码任务")
      (aider/switch-provider "deepseek"))
     ((string= best-model "deepseek-chat")
      (message "推荐使用 DeepSeek Chat 模型处理分析任务")
      (aider/switch-provider "deepseek"))
     (t
      (message "推荐使用 OpenAI GPT-4 模型")
      (aider/switch-provider "openai")))))

(provide 'funcs)
;;; funcs.el ends here
