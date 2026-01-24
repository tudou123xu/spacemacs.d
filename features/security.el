;;; security.el --- 安全审计模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024
;; Description: 提供配置变更审计和安全检查功能

;; ==================== 常量定义 ====================
(defconst my/config-audit-max-size (* 10 1024 1024)
  "审计日志最大大小 (10MB)")

;; ==================== 变量定义 ====================
(defvar my/config-audit-log 
  (expand-file-name "logs/config-audit.log" user-emacs-directory)
  "配置变更审计日志文件路径")

(defvar my/audit-enabled t
  "是否启用审计功能")

;; ==================== 审计日志管理 ====================
(defun my/ensure-log-directory ()
  "确保日志目录存在并设置权限"
  (let ((log-dir (file-name-directory my/config-audit-log)))
    (unless (file-exists-p log-dir)
      (make-directory log-dir t)
      (message "创建审计日志目录: %s" log-dir))
    
    (when (file-exists-p my/config-audit-log)
      (condition-case err
          (set-file-modes my/config-audit-log #o600)
        (error
         (message "设置审计日志权限失败: %s" (error-message-string err)))))))

;; ==================== 配置变更记录 ====================
(defun my/log-config-change ()
  "记录配置文件变更"
  (when (and my/audit-enabled
             buffer-file-name
             (string-match-p "\\.spacemacs\\.d" buffer-file-name)
             (string-suffix-p ".el" buffer-file-name))
    (condition-case err
        (progn
          (my/ensure-log-directory)
          (let ((log-entry (format "[%s] File: %s, User: %s, Size: %d bytes\n"
                                   (format-time-string "%Y-%m-%d %H:%M:%S")
                                   (file-name-nondirectory buffer-file-name)
                                   (user-real-login-name)
                                   (buffer-size))))
            (append-to-file log-entry nil my/config-audit-log)))
      (error 
       (message "审计日志记录失败: %s" (error-message-string err))))))

;; ==================== 安全检查 ====================
(defun my/security-check ()
  "执行基础安全检查"
  (interactive)
  (message "执行安全检查...")
  
  (let ((issues '()))
    
    ;; 检查审计日志权限
    (when (file-exists-p my/config-audit-log)
      (let ((modes (file-modes my/config-audit-log)))
        (unless (eq modes #o600)
          (condition-case err
              (progn
                (set-file-modes my/config-audit-log #o600)
                (message "已修复审计日志文件权限"))
            (error
             (push (format "无法修复审计日志权限: %s" (error-message-string err)) 
                   issues))))))
    
    ;; 检查敏感目录权限
    (let ((sensitive-dirs (list (expand-file-name "logs/" user-emacs-directory))))
      (dolist (dir sensitive-dirs)
        (when (file-exists-p dir)
          (condition-case err
              (set-file-modes dir #o700)
            (error
             (push (format "无法设置目录权限 %s: %s" dir (error-message-string err))
                   issues))))))
    
    ;; 显示结果
    (if issues
        (progn
          (message "⚠ 发现 %d 个安全问题:" (length issues))
          (dolist (issue issues)
            (message "  - %s" issue)))
      (message "✓ 安全检查通过"))))

;; ==================== 审计报告 ====================
(defun my/generate-audit-report ()
  "生成审计报告"
  (interactive)
  (let ((report-file (expand-file-name "logs/audit-report.txt" user-emacs-directory)))
    (condition-case err
        (with-temp-file report-file
          (insert "==================== 安全审计报告 ====================\n\n")
          (insert (format "生成时间: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
          (insert (format "用户: %s\n" (user-real-login-name)))
          (insert (format "系统: %s\n\n" (my/get-system-name)))
          
          (insert "==================== 审计日志统计 ====================\n")
          (if (file-exists-p my/config-audit-log)
              (let ((log-size (file-attribute-size (file-attributes my/config-audit-log))))
                (insert (format "日志文件大小: %.2f KB\n" (/ log-size 1024.0)))
                (insert (format "日志文件路径: %s\n\n" my/config-audit-log))
                
                (insert "==================== 最近配置变更 ====================\n")
                (insert-file-contents my/config-audit-log nil 
                                      (max 0 (- log-size 3000)) nil))
            (insert "审计日志文件不存在\n")))
        (message "✓ 审计报告已生成: %s" report-file))
      (error
       (message "✗ 生成审计报告失败: %s" (error-message-string err))))))

;; ==================== 初始化 ====================
(defun my/init-security-audit ()
  "初始化安全审计模块"
  (message "初始化安全审计模块...")
  
  ;; 确保日志目录存在
  (my/ensure-log-directory)
  
  ;; 执行安全检查
  (my/security-check)
  
  ;; 添加保存钩子
  (add-hook 'after-save-hook #'my/log-config-change)
  
  (message "✓ 安全审计模块初始化完成"))

;; 延迟初始化（等待 common 模块）
(run-with-timer 2.5 nil #'my/init-security-audit)

(provide 'security)
;;; security.el ends here