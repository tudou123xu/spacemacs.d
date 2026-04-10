;;; user-config.el --- 分层配置入口 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024
;; Description: 用户自定义配置的入口文件，加载配置管理器

;; ==================== 加载配置管理器 ====================
(add-to-list 'load-path "~/.spacemacs.d/")

(when (file-exists-p "~/.spacemacs.d/config-manager.el")
  (condition-case err
      (progn
        (load-file "~/.spacemacs.d/config-manager.el")
        (message "✓ 分层配置管理器已加载"))
    (error
     (message "✗ 分层配置管理器加载失败: %s" (error-message-string err)))))

;; ==================== 用户自定义配置区域 ====================
;; 在此处添加您的个人配置代码

;; ==================== Org-roam 配置 ====================
(setq org-roam-v2-ack t)

(with-eval-after-load 'org-roam
  (setq org-roam-directory (file-truename "~/org-notes/org-roam")
        org-roam-completion-everywhere t)
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  (org-roam-setup))

;; 示例：自定义键绑定
;; (global-set-key (kbd "C-c C-c") 'comment-region)

;; 示例：自定义变量
;; (setq custom-variable "custom-value")

;; 示例：自定义函数
;; (defun my/custom-function ()
;;   "自定义函数说明"
;;   (interactive)
;;   (message "执行自定义功能"))

;; ==================== 配置加载完成 ====================
(message "✓ 用户配置加载完成")

(provide 'user-config)
;;; user-config.el ends here