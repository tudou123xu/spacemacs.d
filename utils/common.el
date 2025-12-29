;;; common.el --- 通用工具模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024
;; Description: 提供系统检测、模块加载、错误处理等通用工具函数

;; ==================== 配置常量 ====================
(defconst my/config-paths
  '(:core "~/.spacemacs.d/core/"
    :features "~/.spacemacs.d/features/"
    :system "~/.spacemacs.d/system/"
    :utils "~/.spacemacs.d/utils/"
    :scripts "~/.spacemacs.d/scripts/"
    :logs "~/.spacemacs.d/logs/")
  "配置路径常量映射表")

;; ==================== 系统检测函数 ====================
(defun my/system-is-mac ()
  "检测是否为 macOS 系统"
  (eq system-type 'darwin))

(defun my/system-is-linux ()
  "检测是否为 Linux 系统"
  (eq system-type 'gnu/linux))

(defun my/system-is-windows ()
  "检测是否为 Windows 系统"
  (memq system-type '(windows-nt ms-dos cygwin)))

(defun my/get-system-name ()
  "获取系统名称字符串"
  (cond
   ((my/system-is-mac) "macOS")
   ((my/system-is-linux) "Linux")
   ((my/system-is-windows) "Windows")
   (t (symbol-name system-type))))

;; ==================== 模块加载器 ====================
(defun my/load-config-module (module-name &optional module-path noerror)
  "安全加载配置模块
参数:
  MODULE-NAME - 模块名称
  MODULE-PATH - 可选的模块路径，默认为 ~/.spacemacs.d/modules/
  NOERROR - 是否忽略错误
返回:
  t - 加载成功
  nil - 加载失败"
  (let* ((path (or module-path "~/.spacemacs.d/modules/"))
         (module-file (expand-file-name (concat module-name ".el") path)))
    
    (if (not (file-exists-p module-file))
        (progn
          (unless noerror
            (message "✗ 模块文件不存在: %s" module-file))
          nil)
      (condition-case err
          (progn
            (load-file module-file)
            (message "✓ 模块 %s 加载成功" module-name)
            t)
        (error 
         (message "✗ 模块 %s 加载失败: %s" module-name (error-message-string err))
         (unless noerror
           (signal (car err) (cdr err)))
         nil)))))

(defun my/require-feature (feature &optional filename noerror)
  "安全加载 feature，带错误处理
参数:
  FEATURE - feature 符号
  FILENAME - 可选的文件名
  NOERROR - 是否忽略错误
返回:
  feature 符号或 nil"
  (condition-case err
      (require feature filename noerror)
    (error
     (message "✗ Feature %s 加载失败: %s" feature (error-message-string err))
     (unless noerror
       (signal (car err) (cdr err)))
     nil)))

;; ==================== 配置验证 ====================
(defun my/validate-module (module-name &optional required module-path)
  "验证模块是否可用
参数:
  MODULE-NAME - 模块名称
  REQUIRED - 是否为必需模块
  MODULE-PATH - 可选的模块路径
返回:
  t - 模块可用
  nil - 模块不可用"
  (let* ((path (or module-path "~/.spacemacs.d/modules/"))
         (module-file (expand-file-name (concat module-name ".el") path)))
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

(defun my/check-dependencies ()
  "检查系统依赖和环境配置
返回:
  问题列表，如果无问题则返回 nil"
  (let ((issues '()))
    
    ;; 检查 Emacs 版本
    (when (version< emacs-version "27.1")
      (push "Emacs 版本过低，建议升级到 27.1 或更高版本" issues))
    
    ;; 检查关键目录
    (dolist (path-entry '(:core :features :system :utils))
      (let ((path (my/get-config-path path-entry)))
        (unless (file-exists-p path)
          (push (format "关键目录不存在: %s" path) issues))))
    
    ;; 返回问题列表
    issues))

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
  "获取配置路径
参数:
  PATH-TYPE - 路径类型关键字，如 :core, :features 等
返回:
  路径字符串"
  (or (plist-get my/config-paths path-type)
      (error "未知的路径类型: %s" path-type)))

(defun my/ensure-directory (path)
  "确保目录存在，不存在则创建
参数:
  PATH - 目录路径
返回:
  t - 目录存在或创建成功"
  (unless (file-exists-p path)
    (condition-case err
        (progn
          (make-directory path t)
          (message "✓ 创建目录: %s" path)
          t)
      (error
       (message "✗ 创建目录失败 %s: %s" path (error-message-string err))
       nil))))

(defun my/expand-config-path (relative-path &optional base-type)
  "扩展相对路径为绝对路径
参数:
  RELATIVE-PATH - 相对路径
  BASE-TYPE - 基础路径类型，默认为项目根目录
返回:
  绝对路径字符串"
  (let ((base (if base-type
                  (my/get-config-path base-type)
                "~/.spacemacs.d/")))
    (expand-file-name relative-path base)))

;; ==================== 统一错误处理 ====================
(defun my/safe-execute (func &optional fallback-func error-message)
  "安全执行函数，失败时执行备用函数
参数:
  FUNC - 要执行的函数
  FALLBACK-FUNC - 可选的备用函数
  ERROR-MESSAGE - 可选的错误消息
返回:
  函数执行结果或 nil"
  (condition-case err
      (funcall func)
    (error 
     (my/log-message 'error 
                     (or error-message "函数执行失败") 
                     (error-message-string err))
     (when fallback-func
       (condition-case fallback-err
           (funcall fallback-func)
         (error 
          (my/log-message 'error 
                         "备用函数也失败" 
                         (error-message-string fallback-err))
          nil))))))

(defun my/safe-execute-with-retry (func &optional retry-count delay)
  "带重试机制的安全函数执行
参数:
  FUNC - 要执行的函数
  RETRY-COUNT - 重试次数，默认为 3
  DELAY - 重试延迟（秒），默认为 1
返回:
  t - 执行成功
  nil - 执行失败"
  (let ((retries (or retry-count 3))
        (delay-time (or delay 1))
        (success nil))
    
    (while (and (> retries 0) (not success))
      (condition-case err
          (progn
            (funcall func)
            (setq success t))
        (error
         (my/log-message 'warning 
                        (format "函数执行失败，剩余重试: %d" retries)
                        (error-message-string err))
         (setq retries (1- retries))
         (when (> retries 0)
           (sleep-for delay-time)))))
    
    success))

(defun my/safe-require (package &optional noerror)
  "安全加载包，记录加载错误
参数:
  PACKAGE - 包名（symbol）
  NOERROR - 是否忽略错误
返回:
  包的 feature 符号或 nil"
  (condition-case err
      (require package nil noerror)
    (error 
     (my/log-message 'error 
                    (format "包加载失败: %s" package) 
                    (error-message-string err))
     (unless noerror
       (signal (car err) (cdr err)))
     nil)))

(defun my/safe-load-file (file-path &optional noerror)
  "安全加载文件，记录加载错误
参数:
  FILE-PATH - 文件路径
  NOERROR - 是否忽略错误
返回:
  t - 加载成功
  nil - 加载失败"
  (if (not (file-exists-p file-path))
      (progn
        (unless noerror
          (my/log-message 'error (format "文件不存在: %s" file-path)))
        nil)
    (condition-case err
        (progn
          (load-file file-path)
          t)
      (error 
       (my/log-message 'error 
                      (format "文件加载失败: %s" file-path) 
                      (error-message-string err))
       (unless noerror
         (signal (car err) (cdr err)))
       nil))))

;; ==================== 日志工具 ====================
(defun my/log-message (level message &optional data)
  "记录日志消息到文件
参数:
  LEVEL - 日志级别: info, warning, error
  MESSAGE - 日志消息
  DATA - 可选的附加数据"
  (let ((log-file (expand-file-name "config.log" (my/get-config-path :logs))))
    (my/ensure-directory (my/get-config-path :logs))
    
    (condition-case err
        (let ((log-entry (format "[%s] %s: %s%s\n"
                                (format-time-string "%Y-%m-%d %H:%M:%S")
                                (upcase (symbol-name level))
                                message
                                (if data (format " | %s" data) ""))))
          (append-to-file log-entry nil log-file))
      (error
       (message "日志记录失败: %s" (error-message-string err))))))

;; ==================== 初始化 ====================
(defun my/init-common ()
  "初始化通用模块"
  (message "初始化通用模块...")
  
  ;; 确保关键目录存在
  (dolist (path-entry '(:core :features :system :utils :logs))
    (my/ensure-directory (my/get-config-path path-entry)))
  
  ;; 检查依赖
  (let ((issues (my/check-dependencies)))
    (when issues
      (message "⚠ 发现 %d 个依赖问题:" (length issues))
      (dolist (issue issues)
        (message "  - %s" issue))))
  
  (message "✓ 通用模块初始化完成"))

;; 延迟初始化
(run-with-timer 0.5 nil #'my/init-common)

;; ==================== 提供模块 ====================
(provide 'common)
;;; common.el ends here
