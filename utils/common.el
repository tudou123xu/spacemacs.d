;;; common.el --- 通用工具模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 系统检测函数 ====================
(defun my/system-is-mac ()
  "检测是否为 macOS 系统"
  (eq system-type 'darwin))

(defun my/system-is-linux ()
  "检测是否为 Linux 系统"
  (eq system-type 'gnu/linux))

(defun my/system-is-windows ()
  "检测是否为 Windows 系统"
  (eq system-type 'windows-nt))

;; ==================== 模块加载器 ====================
(defun my/load-config-module (module-name &optional module-path)
  "安全加载配置模块"
  (let ((path (or module-path "~/.spacemacs.d/modules/"))
        (module-file (expand-file-name (concat module-name ".el") path)))
    (when (file-exists-p module-file)
      (condition-case err
          (progn
            (load-file module-file)
            (message "✓ 模块 %s 加载成功" module-name)
            t)
        (error 
         (message "✗ 模块 %s 加载失败: %s" module-name (error-message-string err))
         nil)))))

;; ==================== 配置验证 ====================
(defun my/validate-module (module-name &optional required)
  "验证模块是否可用"
  (let ((module-file (expand-file-name (concat module-name ".el") "~/.spacemacs.d/modules/")))
    (cond
     ((not (file-exists-p module-file))
      (if required
          (progn
            (message "✗ 必需模块 %s 缺失" module-name)
            nil)
        (message "⚠ 可选模块 %s 不存在" module-name)
        nil))
     (t
      (message "✓ 模块 %s 可用" module-name)
      t))))

;; ==================== 配置常量 ====================
(defconst my/config-paths
  '(:core "~/.spacemacs.d/core/"
    :features "~/.spacemacs.d/features/"
    :system "~/.spacemacs.d/system/"
    :utils "~/.spacemacs.d/utils/"
    :scripts "~/.spacemacs.d/scripts/"
    :logs "~/.spacemacs.d/logs/")
  "配置路径常量")

;; ==================== 路径工具 ====================
(defun my/get-config-path (path-type)
  "获取配置路径"
  (plist-get my/config-paths path-type))

(defun my/ensure-directory (path)
  "确保目录存在"
  (unless (file-exists-p path)
    (make-directory path t)
    (message "✓ 创建目录: %s" path)))

;; ==================== 统一错误处理 ====================
(defun my/safe-execute (func &optional fallback-func error-message)
  "安全执行函数，失败时执行备用函数"
  (condition-case err
      (funcall func)
    (error 
     (my/log-message 'error (or error-message "函数执行失败") (error-message-string err))
     (when fallback-func
       (condition-case fallback-err
           (funcall fallback-func)
         (error (my/log-message 'error "备用函数也失败" (error-message-string fallback-err))))))))

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
         (my/log-message 'warning (format "函数执行失败，剩余重试: %d" retries)
                         (error-message-string err))
         (setq retries (1- retries))
         (when (> retries 0)
           (sleep-for delay-time)))))
    success))

(defun my/safe-require (package &optional noerror)
  "安全加载包，记录加载错误"
  (condition-case err
      (require package nil noerror)
    (error 
     (my/log-message 'error (format "包加载失败: %s" package) (error-message-string err))
     (unless noerror
       (signal (car err) (cdr err))))))

(defun my/safe-load-file (file-path &optional noerror)
  "安全加载文件，记录加载错误"
  (condition-case err
      (load-file file-path)
    (error 
     (my/log-message 'error (format "文件加载失败: %s" file-path) (error-message-string err))
     (unless noerror
       (signal (car err) (cdr err))))))

;; ==================== 日志工具 ====================
(defun my/log-message (level message &optional data)
  "记录日志消息"
  (let ((log-file (expand-file-name "config.log" (my/get-config-path :logs))))
    (my/ensure-directory (my/get-config-path :logs))
    (let ((log-entry (format "[%s] %s: %s%s\n"
                             (format-time-string "%Y-%m-%d %H:%M:%S")
                             (upcase (symbol-name level))
                             message
                             (if data (format " | Data: %s" data) ""))))
      (append-to-file log-entry nil log-file))))

;; ==================== 提供模块 ====================
(provide 'common)
;;; common.el ends here
