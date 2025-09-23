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

;; ==================== 日志目录管理 ====================
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

;; ==================== 错误日志记录 ====================
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

;; ==================== 安全函数执行 ====================
;; 使用 common.el 中的统一实现

(defun my/safe-execute-with-retry (func retry-count &optional delay)
  "带重试机制的安全函数执行"
  (let ((retries (or retry-count 3))
        (delay-time (or delay 1))
        (success nil))
    (while (and (> retries 0) (not success))
      (condition-case err
          (progn
            (funcall func)
            (setq success t))
        (error
         (my/log-warning (format "函数执行失败，剩余重试: %d" retries)
                         (error-message-string err))
         (setq retries (1- retries))
         (when (> retries 0)
           (sleep-for delay-time)))))
    success))

;; ==================== 包加载错误处理 ====================
(defun my/safe-require (package &optional noerror)
  "安全加载包，记录加载错误"
  (condition-case err
      (require package nil noerror)
    (error 
     (my/log-error (format "包加载失败: %s" package) (error-message-string err))
     (unless noerror
       (signal (car err) (cdr err))))))

(defun my/safe-load-file (file-path &optional noerror)
  "安全加载文件，记录加载错误"
  (condition-case err
      (load-file file-path)
    (error 
     (my/log-error (format "文件加载失败: %s" file-path) (error-message-string err))
     (unless noerror
       (signal (car err) (cdr err))))))

;; ==================== 函数定义检查 ====================
(defun my/ensure-function (func-name &optional fallback-func)
  "确保函数已定义，否则使用备用函数"
  (if (fboundp func-name)
      func-name
    (when fallback-func
      (my/log-warning (format "函数 %s 未定义，使用备用函数" func-name))
      fallback-func)))

(defun my/ensure-variable (var-name &optional default-value)
  "确保变量已定义，否则设置默认值"
  (unless (boundp var-name)
    (my/log-warning (format "变量 %s 未定义，设置默认值" var-name))
    (set var-name default-value))
  var-name)

;; ==================== 系统命令检查 ====================
(defun my/check-command (command &optional fallback-command)
  "检查系统命令是否可用，返回可用的命令"
  (let ((available-command (or (executable-find command) fallback-command)))
    (unless available-command
      (my/log-warning (format "系统命令不可用: %s" command)))
    available-command))

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

;; ==================== 全局错误处理器 ====================
(defun my/global-error-handler (error-data context)
  "全局错误处理器"
  (my/log-error (format "全局错误: %s" context) error-data)
  
  ;; 尝试恢复
  (my/recover-from-error)
  
  ;; 显示用户友好的错误消息
  (message "发生错误，已记录到日志文件。请检查 %s" my/error-log-file))

;; ==================== 错误统计 ====================
(defvar my/error-stats
  '((total-errors . 0)
    (total-warnings . 0)
    (last-error-time . nil))
  "错误统计信息")

(defun my/update-error-stats (type)
  "更新错误统计"
  (setq my/error-stats
        (plist-put my/error-stats :last-error-time (current-time)))
  (if (eq type 'error)
      (setq my/error-stats
            (plist-put my/error-stats :total-errors 
                       (1+ (plist-get my/error-stats :total-errors))))
    (setq my/error-stats
          (plist-put my/error-stats :total-warnings 
                     (1+ (plist-get my/error-stats :total-warnings))))))

;; ==================== 错误报告 ====================
(defun my/generate-error-report ()
  "生成错误报告"
  (interactive)
  (let ((report-file (expand-file-name "logs/error-report.txt" user-emacs-directory)))
    (with-temp-file report-file
      (insert (format "Emacs 错误报告\n"))
      (insert (format "生成时间: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "Emacs 版本: %s\n" emacs-version))
      (insert (format "系统类型: %s\n" system-type))
      (insert (format "总错误数: %d\n" (plist-get my/error-stats :total-errors)))
      (insert (format "总警告数: %d\n" (plist-get my/error-stats :total-warnings)))
      (insert (format "最后错误时间: %s\n" 
                      (if (plist-get my/error-stats :last-error-time)
                          (format-time-string "%Y-%m-%d %H:%M:%S" 
                                              (plist-get my/error-stats :last-error-time))
                        "无"))))
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