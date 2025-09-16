;;; windows-specific.el --- Windows 专属配置 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== Windows 系统集成 ====================
(when (my/system-is-windows)
  
  ;; 系统快捷键映射
  (setq w32-pass-alt-to-system nil
        w32-apps-modifier 'super)
  
  ;; 文件系统优化
  (setq w32-get-true-file-attributes nil
        w32-pipe-read-delay 0)
  
  ;; 字体配置
  (set-face-attribute 'default nil :font "Consolas-14")
  
  ;; 系统服务集成
  (defun windows/notify (title message)
    "发送 Windows 通知"
    (when (executable-find "powershell")
      (call-process "powershell" nil nil nil
                    "-Command" (format "New-BurntToastNotification -Text '%s' -AppLogo '%s'" 
                                      message title))))
  
  (defun windows/reveal-in-explorer ()
    "在资源管理器中显示当前文件"
    (interactive)
    (when buffer-file-name
      (w32-shell-execute "explore" (file-name-directory buffer-file-name))))
  
  ;; 系统级快捷键
  (global-set-key (kbd "s-r") #'windows/reveal-in-explorer)
  (global-set-key (kbd "s-w") #'delete-window)
  (global-set-key (kbd "s-q") #'save-buffers-kill-terminal)
  
  ;; 性能优化
  (setq w32-use-visible-system-caret nil
        w32-mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  
  (message "Windows 专属配置已加载"))

(provide 'windows-specific)
;;; windows-specific.el ends here