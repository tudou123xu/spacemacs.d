;;; error-handling.el --- 错误处理和容错模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 错误处理配置 ====================
(defvar my/error-log-file 
  (expand-file-name "logs/error.log" user-emacs-directory)
  "错误日志文件路径")

(defvar my/error-handling-enabled t
  "是否启用错误处理")

(defvar my/error-log-max-size (* 10 1024 1024)  ; 10MB
  "错误日志最大大小")

;; ==================== 日志管理 ====================
(defun my/ensure-error-log-directory ()
  "确保错误日志目录存在"
  (let ((log-dir (file-name-directory my/error-log-file)))
    (unless (file-exists-p log-dir)
      (make-directory log-dir t))
    (when (file-exists-p my/error-log-file)
      (set-file-modes my/error-log-file #o600))))

(defun my/rotate-error-log-if-needed ()
  "如果错误日志过大则轮转"
  (when (and (file-exists-p my/error-log-file)
             (> (file-attribute-size (file-attributes my/error-log-file))
                my/error-log-max-size))
    (let ((backup-file (concat my/error-log-file ".backup")))
      (rename-file my/error-log-file backup-file t)
      (message "错误日志已轮转: %s" backup-file))))

(defun my/log-error (error-message &optional error-data)
  "记录错误到日志文件"
  (when my/error-handling-enabled
    (condition-case err
        (progn
          (my/ensure-error-log-directory)
          (my/rotate-error-log-if-needed)
          (let ((log-entry (format "[%s] ERROR: %s%s\n"
                                   (format-time-string "%Y-%m-%d %H:%M:%S")
                                   error-message
                                   (if error-data (format " | Data: %s" error-data) ""))))
            (append-to-file log-entry nil my/error-log-file)))
      (error (message "错误日志记录失败: %s" (error-message-string err))))))

(defun my/log-warning (warning-message &optional warning-data)
  "记录警告到日志文件"
  (when my/error-handling-enabled
    (condition-case err
        (progn
          (my/ensure-error-log-directory)
          (let ((log-entry (format "[%s] WARNING: %s%s\n"
                                   (format-time-string "%Y-%m-%d %H:%M:%S")
                                   warning-message
                                   (if warning-data (format " | Data: %s" warning-data) ""))))
            (append-to-file log-entry nil my/error-log-file)))
      (error (message "警告日志记录失败: %s" (error-message-string err))))))

;; ==================== 错误恢复机制 ====================
(defun my/recover-from-error ()
  "从错误中恢复的基本机制"
  (message "尝试从错误中恢复...")
  
  ;; 清理可能的损坏状态
  (when (and (boundp 'gc-cons-threshold) 
             (> gc-cons-threshold most-positive-fixnum))
    (setq gc-cons-threshold 16777216)  ; 恢复正常的 GC 设置
    (message "已恢复 GC 设置"))
  
  ;; 重新初始化关键模块
  (condition-case err
      (progn
        (my/ensure-error-log-directory)
        (message "错误恢复完成"))
    (error (message "错误恢复失败: %s" (error-message-string err)))))

;; ==================== 错误报告 ====================
(defun my/generate-error-report ()
  "生成错误报告"
  (interactive)
  (let ((report-file (expand-file-name "logs/error-report.txt" user-emacs-directory)))
    (with-temp-file report-file
      (insert (format "Emacs 错误报告\n"))
      (insert (format "生成时间: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "Emacs 版本: %s\n" emacs-version))
      (insert (format "系统类型: %s\n" system-type)))
    (message "错误报告已生成: %s" report-file)))

;; ==================== 初始化 ====================
(defun my/init-error-handling ()
  "初始化错误处理模块"
  (message "初始化错误处理模块...")
  
  ;; 确保日志目录存在
  (my/ensure-error-log-directory)
  
  ;; 设置全局错误处理
  (add-hook 'after-init-hook 
            (lambda ()
              (my/ensure-error-log-directory)
              (message "错误处理模块已初始化")))
  
  (message "错误处理模块初始化完成"))

;; 立即初始化
(my/init-error-handling)

(provide 'error-handling)
;;; error-handling.el ends here