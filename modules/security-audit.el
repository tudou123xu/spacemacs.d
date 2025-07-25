;;; security-audit.el --- 安全审计模块 -*- lexical-binding: t; -*-

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
          (let ((log-entry (format "[%s] File: %s, User: %s, Hash: %s\n"
                                   (format-time-string "%Y-%m-%d %H:%M:%S")
                                   (file-name-nondirectory buffer-file-name)
                                   (user-real-login-name)
                                   (secure-hash 'md5 (current-buffer)))))
            (append-to-file log-entry nil my/config-audit-log)))
      (error (message "审计日志记录失败: %s" (error-message-string err))))))

(add-hook 'after-save-hook #'my/log-config-change)

;; ==================== 安全检查 ====================
(defun my/security-check ()
  "基础安全检查"
  (when (and (file-exists-p my/config-audit-log)
             (not (eq (file-modes my/config-audit-log) #o600)))
    (set-file-modes my/config-audit-log #o600)
    (message "已修复审计日志文件权限")))

(add-hook 'after-init-hook #'my/security-check)

;; ==================== 敏感信息保护 ====================
(defun my/mask-sensitive-info (text)
  "遮蔽敏感信息"
  (let ((patterns '(("password[[:space:]]*=[[:space:]]*\\([^[:space:]]+\\)" . "password=****")
                    ("token[[:space:]]*=[[:space:]]*\\([^[:space:]]+\\)" . "token=****")
                    ("api[_-]?key[[:space:]]*=[[:space:]]*\\([^[:space:]]+\\)" . "api_key=****"))))
    (dolist (pattern patterns)
      (setq text (replace-regexp-in-string (car pattern) (cdr pattern) text)))
    text))

(provide 'security-audit)
;;; security-audit.el ends here 