;;; validate-config.el --- Spacemacs 配置验证工具 -*- lexical-binding: t; -*-

;; ==================== 配置验证工具 ====================
(defvar validate-errors '()
  "配置验证错误列表")

(defvar validate-warnings '()
  "配置验证警告列表")

(defun validate/add-error (message)
  "添加错误信息"
  (push message validate-errors))

(defun validate/add-warning (message)
  "添加警告信息"
  (push message validate-warnings))

;; ==================== 文件存在性检查 ====================
(defun validate/check-file-existence ()
  "检查关键配置文件是否存在"
  (let ((required-files '("init.el"
                          "user-config.el"
                          ".spacemacs.env"
                          "modules/core-performance.el"
                          "modules/lang-support.el"
                          "modules/ui-enhancement.el"
                          "modules/system-integration.el"
                          "modules/security-audit.el"
                          "private/aider/packages.el"
                          "private/aider/config.el"
                          "private/aider/funcs.el"
                          "private/aider/keybindings.el")))
    (dolist (file required-files)
      (let ((full-path (expand-file-name file "~/.spacemacs.d/")))
        (unless (file-exists-p full-path)
          (validate/add-error (format "缺少必需文件: %s" file)))))))

;; ==================== 模块加载测试 ====================
(defun validate/test-module-loading ()
  "测试模块是否能正常加载"
  (let ((modules '("core-performance"
                   "lang-support"
                   "ui-enhancement"
                   "system-integration"
                   "security-audit")))
    (dolist (module modules)
      (condition-case err
          (let ((module-path (expand-file-name 
                             (concat "modules/" module ".el")
                             "~/.spacemacs.d/")))
            (when (file-exists-p module-path)
              (load-file module-path)
              (message "✓ 模块 %s 加载成功" module)))
        (error 
         (validate/add-error 
          (format "模块 %s 加载失败: %s" module (error-message-string err))))))))

;; ==================== 函数定义检查 ====================
(defun validate/check-functions ()
  "检查关键函数是否正确定义"
  (let ((required-functions '(my/number-of-processors
                              my/load-config-module
                              aider/start-session
                              aider/add-current-file)))
    (dolist (func required-functions)
      (unless (fboundp func)
        (validate/add-warning (format "函数 %s 未定义" func))))))

;; ==================== 变量检查 ====================
(defun validate/check-variables ()
  "检查关键变量是否正确设置"
  (let ((required-vars '(my/config-modules-path
                         my/config-audit-log)))
    (dolist (var required-vars)
      (unless (boundp var)
        (validate/add-warning (format "变量 %s 未定义" var))))))

;; ==================== 平台专属检查 ====================
(defun validate/check-platform-specific ()
  "检查平台专属配置"
  (when (spacemacs/system-is-mac)
    (let ((macos-file (expand-file-name "modules/macos-specific.el" "~/.spacemacs.d/")))
      (unless (file-exists-p macos-file)
        (validate/add-warning "macOS 平台缺少专属配置文件"))))
  
  (when (spacemacs/system-is-mswindows)
    (let ((windows-file (expand-file-name "modules/windows-specific.el" "~/.spacemacs.d/")))
      (unless (file-exists-p windows-file)
        (validate/add-warning "Windows 平台缺少专属配置文件")))))

;; ==================== 主验证函数 ====================
(defun validate/run-all-checks ()
  "运行所有配置验证检查"
  (interactive)
  (setq validate-errors '()
        validate-warnings '())
  
  (message "开始 Spacemacs 配置验证...")
  
  ;; 运行各项检查
  (validate/check-file-existence)
  (validate/test-module-loading)
  (validate/check-functions)
  (validate/check-variables)
  (validate/check-platform-specific)
  
  ;; 显示结果
  (validate/display-results))

(defun validate/display-results ()
  "显示验证结果"
  (let ((error-count (length validate-errors))
        (warning-count (length validate-warnings)))
    
    (message "\n==================== 验证结果 ====================")
    
    (if (= error-count 0)
        (message "✓ 所有关键检查通过！")
      (message "✗ 发现 %d 个错误：" error-count)
      (dolist (error validate-errors)
        (message "  - %s" error)))
    
    (when (> warning-count 0)
      (message "\n⚠ 发现 %d 个警告：" warning-count)
      (dolist (warning validate-warnings)
        (message "  - %s" warning)))
    
    (message "\n==================== 验证完成 ====================")
    
    ;; 返回验证状态
    (= error-count 0)))

;; ==================== 自动修复功能 ====================
(defun validate/auto-fix ()
  "尝试自动修复一些常见问题"
  (interactive)
  (message "尝试自动修复配置问题...")
  
  ;; 创建缺失的目录
  (let ((dirs '("modules" "private/aider" "private/ellama" "private/db-layer" "logs")))
    (dolist (dir dirs)
      (let ((full-path (expand-file-name dir "~/.spacemacs.d/")))
        (unless (file-directory-p full-path)
          (make-directory full-path t)
          (message "✓ 创建目录: %s" dir)))))
  
  ;; 设置日志文件权限
  (let ((log-file (expand-file-name "logs/config-audit.log" "~/.spacemacs.d/")))
    (when (file-exists-p log-file)
      (set-file-modes log-file #o600)
      (message "✓ 修复日志文件权限")))
  
  (message "自动修复完成"))

(provide 'validate-config)
;;; validate-config.el ends here 