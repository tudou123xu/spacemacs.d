;;; error-handling.el --- 错误处理和容错模块 -*- lexical-binding: t; -*-

;; ==================== 错误处理配置 ====================
(defvar my/error-log-file 
  (expand-file-name "logs/error.log" user-emacs-directory)
  "错误日志文件路径")

(defvar my/error-handling-enabled t
  "是否启用错误处理")

;; ==================== 日志目录管理 ====================
(defun my/ensure-error-log-directory ()
  "确保错误日志目录存在"
  (let ((log-dir (file-name-directory my/error-log-file)))
    (unless (file-exists-p log-dir)
      (make-directory log-dir t))
    (when (file-exists-p my/error-log-file)
      (set-file-modes my/error-log-file #o600))))

;; ==================== 错误日志记录 ====================
(defun my/log-error (error-message &optional error-data)
  "记录错误到日志文件"
  (when my/error-handling-enabled
    (condition-case err
        (progn
          (my/ensure-error-log-directory)
          (let ((log-entry (format "[%s] ERROR: %s%s\n"
                                   (format-time-string "%Y-%m-%d %H:%M:%S")
                                   error-message
                                   (if error-data (format " | Data: %s" error-data) ""))))
            (append-to-file log-entry nil my/error-log-file)))
      (error (message "错误日志记录失败: %s" (error-message-string err))))))

;; ==================== 安全函数执行 ====================
(defun my/safe-execute (func &optional fallback-func error-message)
  "安全执行函数，失败时执行备用函数"
  (condition-case err
      (funcall func)
    (error 
     (my/log-error (or error-message (format "函数执行失败: %s" func))
                   (error-message-string err))
     (when fallback-func
       (condition-case fallback-err
           (funcall fallback-func)
         (error (my/log-error "备用函数也失败" (error-message-string fallback-err))))))))

;; ==================== 包加载错误处理 ====================
(defun my/safe-require (package &optional noerror)
  "安全加载包，记录加载错误"
  (condition-case err
      (require package nil noerror)
    (error 
     (my/log-error (format "包加载失败: %s" package) (error-message-string err))
     (unless noerror
       (signal (car err) (cdr err))))))

;; ==================== 配置加载错误处理 ====================
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
      (my/log-error (format "函数 %s 未定义，使用备用函数" func-name))
      fallback-func)))

;; ==================== 变量定义检查 ====================
(defun my/ensure-variable (var-name &optional default-value)
  "确保变量已定义，否则设置默认值"
  (unless (boundp var-name)
    (my/log-error (format "变量 %s 未定义，设置默认值" var-name))
    (set var-name default-value))
  var-name)

;; ==================== 系统命令检查 ====================
(defun my/check-command (command &optional fallback-command)
  "检查系统命令是否可用，返回可用的命令"
  (let ((available-command (or (executable-find command) fallback-command)))
    (unless available-command
      (my/log-error (format "系统命令不可用: %s" command)))
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
        (when (fboundp 'my/ensure-log-directory)
          (my/ensure-log-directory))
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

;; ==================== 初始化错误处理 ====================
(add-hook 'after-init-hook 
          (lambda ()
            (my/ensure-error-log-directory)
            (message "错误处理模块已初始化")))

(provide 'error-handling)
;;; error-handling.el ends here
