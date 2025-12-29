;;; error-handling.el --- 错误处理和容错模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024
;; Description: 提供统一的错误处理、日志记录和恢复机制

;; ==================== 常量定义 ====================
(defconst my/error-log-max-size (* 10 1024 1024)
  "错误日志最大大小 (10MB)")

(defconst my/error-log-backup-count 5
  "错误日志备份保留数量")

;; ==================== 变量定义 ====================
(defvar my/error-log-file 
  (expand-file-name "logs/error.log" user-emacs-directory)
  "错误日志文件路径")

(defvar my/error-handling-enabled t
  "是否启用错误处理")

(defvar my/error-count 0
  "错误计数器")

(defvar my/warning-count 0
  "警告计数器")

;; ==================== 日志管理 ====================
(defun my/ensure-error-log-directory ()
  "确保错误日志目录存在，并设置正确的文件权限"
  (let ((log-dir (file-name-directory my/error-log-file)))
    (unless (file-exists-p log-dir)
      (make-directory log-dir t)
      (message "创建日志目录: %s" log-dir))
    
    ;; 设置日志文件权限
    (when (file-exists-p my/error-log-file)
      (condition-case err
          (set-file-modes my/error-log-file #o600)
        (error
         (message "设置日志文件权限失败: %s" (error-message-string err)))))))

(defun my/rotate-error-log-if-needed ()
  "如果错误日志过大则进行轮转，保留备份"
  (when (and (file-exists-p my/error-log-file)
             (> (file-attribute-size (file-attributes my/error-log-file))
                my/error-log-max-size))
    
    ;; 清理旧备份
    (my/cleanup-old-log-backups)
    
    ;; 创建新备份
    (let ((backup-file (format "%s.%s" 
                               my/error-log-file 
                               (format-time-string "%Y%m%d%H%M%S"))))
      (condition-case err
          (progn
            (rename-file my/error-log-file backup-file t)
            (message "错误日志已轮转: %s" backup-file))
        (error
         (message "日志轮转失败: %s" (error-message-string err)))))))

(defun my/cleanup-old-log-backups ()
  "清理旧的日志备份，只保留最近的几个"
  (let* ((log-dir (file-name-directory my/error-log-file))
         (log-name (file-name-nondirectory my/error-log-file))
         (backups (directory-files log-dir t (concat (regexp-quote log-name) "\\..*"))))
    
    (when (> (length backups) my/error-log-backup-count)
      (let ((sorted-backups (sort backups #'file-newer-than-file-p))
            (to-delete (nthcdr my/error-log-backup-count sorted-backups)))
        (dolist (file to-delete)
          (condition-case err
              (progn
                (delete-file file)
                (message "删除旧日志备份: %s" file))
            (error
             (message "删除备份失败: %s - %s" file (error-message-string err)))))))))

(defun my/log-error (error-message &optional error-data context)
  "记录错误到日志文件
参数:
  ERROR-MESSAGE - 错误消息（string）
  ERROR-DATA - 可选的错误数据
  CONTEXT - 可选的上下文信息"
  (when my/error-handling-enabled
    (setq my/error-count (1+ my/error-count))
    (condition-case err
        (progn
          (my/ensure-error-log-directory)
          (my/rotate-error-log-if-needed)
          (let ((log-entry (format "[%s] ERROR #%d: %s%s%s\n"
                                   (format-time-string "%Y-%m-%d %H:%M:%S")
                                   my/error-count
                                   error-message
                                   (if error-data (format " | Data: %s" error-data) "")
                                   (if context (format " | Context: %s" context) ""))))
            (append-to-file log-entry nil my/error-log-file)))
      (error 
       (message "错误日志记录失败: %s" (error-message-string err))))))

(defun my/log-warning (warning-message &optional warning-data context)
  "记录警告到日志文件
参数:
  WARNING-MESSAGE - 警告消息（string）
  WARNING-DATA - 可选的警告数据
  CONTEXT - 可选的上下文信息"
  (when my/error-handling-enabled
    (setq my/warning-count (1+ my/warning-count))
    (condition-case err
        (progn
          (my/ensure-error-log-directory)
          (let ((log-entry (format "[%s] WARNING #%d: %s%s%s\n"
                                   (format-time-string "%Y-%m-%d %H:%M:%S")
                                   my/warning-count
                                   warning-message
                                   (if warning-data (format " | Data: %s" warning-data) "")
                                   (if context (format " | Context: %s" context) ""))))
            (append-to-file log-entry nil my/error-log-file)))
      (error 
       (message "警告日志记录失败: %s" (error-message-string err))))))

(defun my/log-info (info-message &optional info-data)
  "记录信息到日志文件
参数:
  INFO-MESSAGE - 信息消息（string）
  INFO-DATA - 可选的信息数据"
  (when my/error-handling-enabled
    (condition-case err
        (progn
          (my/ensure-error-log-directory)
          (let ((log-entry (format "[%s] INFO: %s%s\n"
                                   (format-time-string "%Y-%m-%d %H:%M:%S")
                                   info-message
                                   (if info-data (format " | Data: %s" info-data) ""))))
            (append-to-file log-entry nil my/error-log-file)))
      (error 
       (message "信息日志记录失败: %s" (error-message-string err))))))

;; ==================== 错误恢复机制 ====================
(defun my/recover-from-error (&optional error-info)
  "从错误中恢复的基本机制
参数:
  ERROR-INFO - 可选的错误信息"
  (message "尝试从错误中恢复...")
  
  (when error-info
    (my/log-error "系统错误恢复" error-info))
  
  ;; 清理可能的损坏状态
  (my/restore-gc-settings)
  
  ;; 重新初始化关键模块
  (condition-case err
      (progn
        (my/ensure-error-log-directory)
        (message "✓ 错误恢复完成"))
    (error 
     (my/log-error "错误恢复失败" (error-message-string err))
     (message "✗ 错误恢复失败: %s" (error-message-string err)))))

(defun my/restore-gc-settings ()
  "恢复正常的垃圾回收设置"
  (when (and (boundp 'gc-cons-threshold) 
             (or (> gc-cons-threshold 268435456)  ; 256MB
                 (< gc-cons-threshold 800000)))    ; 800KB
    (setq gc-cons-threshold 16777216)  ; 恢复到 16MB
    (message "已恢复 GC 设置到正常值: 16MB")))

(defun my/safe-call (func &rest args)
  "安全调用函数，捕获并记录错误
参数:
  FUNC - 要调用的函数
  ARGS - 函数参数"
  (condition-case err
      (apply func args)
    (error
     (my/log-error (format "函数调用失败: %s" func) (error-message-string err))
     nil)))

;; ==================== 错误统计和报告 ====================
(defun my/get-error-statistics ()
  "获取错误统计信息"
  (interactive)
  (message "错误统计: 错误 %d 次, 警告 %d 次" 
           my/error-count my/warning-count)
  (list :errors my/error-count :warnings my/warning-count))

(defun my/reset-error-statistics ()
  "重置错误统计"
  (interactive)
  (setq my/error-count 0
        my/warning-count 0)
  (message "错误统计已重置"))

(defun my/generate-error-report ()
  "生成详细的错误报告"
  (interactive)
  (let ((report-file (expand-file-name "logs/error-report.txt" user-emacs-directory)))
    (condition-case err
        (with-temp-file report-file
          (insert "==================== Emacs 错误报告 ====================\n\n")
          (insert (format "生成时间: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
          (insert (format "Emacs 版本: %s\n" emacs-version))
          (insert (format "系统类型: %s\n" system-type))
          (insert (format "系统名称: %s\n\n" (system-name)))
          
          (insert "==================== 统计信息 ====================\n")
          (insert (format "错误总数: %d\n" my/error-count))
          (insert (format "警告总数: %d\n\n" my/warning-count))
          
          (insert "==================== 系统状态 ====================\n")
          (insert (format "GC 阈值: %s\n" gc-cons-threshold))
          (insert (format "已加载特性: %d 个\n" (length features)))
          (insert (format "进程输出最大值: %s\n\n" read-process-output-max))
          
          (when (file-exists-p my/error-log-file)
            (insert "==================== 最近错误日志 ====================\n")
            (insert-file-contents my/error-log-file nil 
                                  (max 0 (- (file-attribute-size 
                                            (file-attributes my/error-log-file)) 5000))
                                  nil)))
        (message "✓ 错误报告已生成: %s" report-file))
      (error
       (message "✗ 生成错误报告失败: %s" (error-message-string err))))))

;; ==================== 初始化 ====================
(defun my/init-error-handling ()
  "初始化错误处理模块"
  (message "初始化错误处理模块...")
  
  ;; 确保日志目录存在
  (my/ensure-error-log-directory)
  
  ;; 重置统计
  (setq my/error-count 0
        my/warning-count 0)
  
  ;; 设置全局错误处理钩子
  (add-hook 'after-init-hook 
            (lambda ()
              (my/ensure-error-log-directory)
              (my/log-info "Emacs 初始化完成"
                          (format "版本: %s, 系统: %s" emacs-version system-type))
              (message "✓ 错误处理模块已初始化")))
  
  (message "✓ 错误处理模块初始化完成"))

;; 立即初始化
(my/init-error-handling)

(provide 'error-handling)
;;; error-handling.el ends here