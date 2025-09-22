;;; user-config.el --- 分层配置入口 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 分层配置管理器 ====================
;; 加载分层配置管理器
(add-to-list 'load-path "~/.spacemacs.d/")
(when (file-exists-p "~/.spacemacs.d/config-manager.el")
  (load-file "~/.spacemacs.d/config-manager.el")
  (message "✓ 分层配置管理器已加载"))

;; ==================== 用户自定义配置 ====================
;; 在这里添加您的个人配置

;; 示例：自定义键绑定
;; (global-set-key (kbd "C-c C-c") 'comment-region)

;; 示例：自定义变量
;; (setq custom-variable "custom-value")

;; ==================== 配置完成 ====================
(message "✓ 用户配置加载完成")

(provide 'user-config)
;;; user-config.el ends here