;;; user-config.el --- 模块化用户配置入口 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 配置模块路径 ====================
(defvar my/config-modules-path "~/.spacemacs.d/modules/"
  "用户配置模块目录路径")

;; ==================== 模块加载器 ====================
(defun my/load-config-module (module-name)
  "安全加载配置模块"
  (let ((module-file (expand-file-name (concat module-name ".el") my/config-modules-path)))
    (when (file-exists-p module-file)
      (condition-case err
          (progn
            (load-file module-file)
            (message "✓ 模块 %s 加载成功" module-name)
            t)
        (error 
         (message "✗ 模块 %s 加载失败: %s" module-name (error-message-string err))
         nil)))))

;; ==================== 核心模块加载 ====================
(defun my/load-core-modules ()
  "加载核心模块"
  (message "加载核心模块...")
  
  ;; 按依赖顺序加载核心模块
  (let ((core-modules '("core-performance"     ; 性能优化
                        "error-handling"       ; 错误处理
                        "package-fix"          ; 包修复
                        "system-integration"))) ; 系统集成
    
    (dolist (module core-modules)
      (my/load-config-module module))))

;; ==================== 可选模块加载 ====================
(defun my/load-optional-modules ()
  "加载可选模块"
  (message "加载可选模块...")
  
  ;; 加载界面增强
  (my/load-config-module "ui-enhancement")
  
  ;; 加载语言支持
  (my/load-config-module "lang-support")
  
  ;; 加载安全审计
  (my/load-config-module "security-audit"))

;; ==================== 平台专属模块加载 ====================
(defun my/load-platform-modules ()
  "加载平台专属模块"
  (message "加载平台专属模块...")
  
  ;; 检测系统类型并加载对应模块
  (cond
   ((eq system-type 'darwin)  ; macOS
    (my/load-config-module "macos-specific"))
   ((eq system-type 'windows-nt)  ; Windows
    (my/load-config-module "windows-specific"))
   (t
    (message "使用通用平台配置"))))

;; ==================== 数据库模块加载 ====================
(defun my/load-database-modules ()
  "加载数据库相关模块"
  (message "加载数据库模块...")
  
  ;; 加载数据库层配置
  (add-to-list 'load-path "~/.spacemacs.d/private/db-layer")
  (when (file-exists-p "~/.spacemacs.d/private/db-layer/config.el")
    (load-file "~/.spacemacs.d/private/db-layer/config.el")))

;; ==================== 主加载函数 ====================
(defun my/load-all-modules ()
  "加载所有配置模块"
  (message "开始加载用户配置模块...")
  
  ;; 加载核心模块
  (my/load-core-modules)
  
  ;; 延迟加载可选模块（提高启动速度）
  (run-with-timer 1 nil #'my/load-optional-modules)
  
  ;; 延迟加载平台模块
  (run-with-timer 1.5 nil #'my/load-platform-modules)
  
  ;; 延迟加载数据库模块
  (run-with-timer 2 nil #'my/load-database-modules)
  
  (message "用户配置模块加载完成"))

;; ==================== 配置验证 ====================
(defun my/validate-configuration ()
  "验证配置完整性"
  (message "验证配置完整性...")
  
  (let ((required-modules '("core-performance" "error-handling" "package-fix"))
        (missing-modules '()))
    
    (dolist (module required-modules)
      (unless (my/load-config-module module)
        (push module missing-modules)))
    
    (if missing-modules
        (message "警告: 缺少关键模块: %s" (string-join missing-modules ", "))
      (message "✓ 配置验证通过"))))

;; ==================== 初始化 ====================
;; 立即加载所有模块
(my/load-all-modules)

;; 延迟验证配置
(run-with-timer 3 nil #'my/validate-configuration)

(provide 'user-config)
;;; user-config.el ends here