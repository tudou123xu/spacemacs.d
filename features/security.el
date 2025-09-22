;;; security-audit.el --- 安全审计模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 审计日志配置 ====================
(defvar my/config-audit-log 
  (expand-file-name "logs/config-audit.log" user-emacs-directory)
  "配置变更审计日志文件路径")

(defun my/ensure-log-directory ()
  "确保日志目录存在"
  (let ((log-dir (file-name-directory my/config-audit-log)))
    (unless (file-exists-p log-dir)
      (make-directory log-dir t))
    (when (file-exists-p my/config-audit-log)
      (set-file-modes my/config-audit-log #o600))))

;; ==================== 配置变更记录 ====================
(defun my/log-config-change ()
  "记录配置文件变更"
  (when (and buffer-file-name
             (string-match-p "\\.spacemacs\\.d" buffer-file-name)
             (string-suffix-p ".el" buffer-file-name))
    (condition-case err
        (progn
          (my/ensure-log-directory)
          (let ((log-entry (format "[%s] File: %s, User: %s\n"
                                   (format-time-string "%Y-%m-%d %H:%M:%S")
                                   (file-name-nondirectory buffer-file-name)
                                   (user-real-login-name))))
            (append-to-file log-entry nil my/config-audit-log)))
      (error (message "审计日志记录失败: %s" (error-message-string err))))))

;; ==================== 安全检查 ====================
(defun my/security-check ()
  "基础安全检查"
  (when (and (file-exists-p my/config-audit-log)
             (not (eq (file-modes my/config-audit-log) #o600)))
    (set-file-modes my/config-audit-log #o600)
    (message "已修复审计日志文件权限")))

;; ==================== 初始化 ====================
(defun my/init-security-audit ()
  "初始化安全审计模块"
  (message "初始化安全审计模块...")
  
  (my/ensure-log-directory)
  (my/security-check)
  
  ;; 添加保存钩子
  (add-hook 'after-save-hook #'my/log-config-change)
  
  (message "安全审计模块初始化完成"))

;; 延迟初始化
(run-with-timer 2.5 nil #'my/init-security-audit)

(provide 'security-audit)
;;; security-audit.el ends here