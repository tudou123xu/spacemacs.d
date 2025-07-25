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

(provide 'funcs)
;;; funcs.el ends here
