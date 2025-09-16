;;; user-config.el --- 模块化用户配置入口 -*- lexical-binding: t; -*-

;; ==================== 配置文件加载器 ====================
(defvar my/config-modules-path "~/.spacemacs.d/modules/"
  "用户配置模块目录路径")

(defun my/load-config-module (module-name)
  "安全加载配置模块"
  (let ((module-file (expand-file-name (concat module-name ".el") my/config-modules-path)))
    (when (file-exists-p module-file)
      (condition-case err
          (load-file module-file)
        (error (message "加载模块 %s 失败: %s" module-name (error-message-string err)))))))

;; ==================== 核心模块加载 ====================
(mapc #'my/load-config-module
      '("package-fix"          ; 包依赖修复和网络优化
        "core-performance"     ; 性能优化
        "error-handling"       ; 错误处理和容错
        "lang-support"         ; 编程语言支持
        "ui-enhancement"       ; 界面增强
        "system-integration"   ; 系统集成
        "security-audit"))     ; 安全审计

;; ==================== 平台专属配置 ====================
(when (my/system-is-mac)
  (my/load-config-module "macos-specific"))

(when (my/system-is-windows)
  (my/load-config-module "windows-specific"))

;; ==================== 数据库模块加载 ====================
(add-to-list 'load-path "~/.spacemacs.d/private/db-layer")
(require 'config)

(provide 'user-config)
;;; user-config.el ends here
