;;; macos.el --- macOS 专属配置 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024
;; Description: macOS 系统的专属配置和集成

;; ==================== macOS 系统检测 ====================
(when (my/system-is-mac)
  
  ;; ==================== 系统快捷键映射 ====================
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-function-modifier 'hyper)
  
  ;; ==================== 环境变量优化 ====================
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-check-startup-files nil)
  
  ;; ==================== 文件系统优化 ====================
  (setq insert-directory-program "/usr/local/bin/gls"
        dired-use-ls-dired t)
  
  ;; ==================== Spotlight 集成 ====================
  (when (executable-find "mdfind")
    (setq locate-command "mdfind"))
  
  ;; ==================== 系统服务集成 ====================
  (defun macos/notify (title message)
    "发送 macOS 系统通知
参数:
  TITLE - 通知标题
  MESSAGE - 通知内容"
    (when (executable-find "osascript")
      (call-process "osascript" nil nil nil
                    "-e" (format "display notification \"%s\" with title \"%s\""
                                message title))))
  
  (defun macos/reveal-in-finder ()
    "在 Finder 中显示当前文件"
    (interactive)
    (if buffer-file-name
        (call-process "open" nil nil nil "-R" buffer-file-name)
      (message "⚠ 当前缓冲区没有关联文件")))
  
  (defun macos/open-terminal-here ()
    "在当前目录打开终端"
    (interactive)
    (let ((dir (or default-directory "~")))
      (call-process "open" nil nil nil "-a" "Terminal" dir)))
  
  ;; ==================== 系统级快捷键 ====================
  (global-set-key (kbd "s-r") #'macos/reveal-in-finder)
  (global-set-key (kbd "s-w") #'delete-window)
  (global-set-key (kbd "s-q") #'save-buffers-kill-terminal)
  (global-set-key (kbd "s-t") #'macos/open-terminal-here)
  
  ;; ==================== Homebrew 集成 ====================
  (when (file-directory-p "/opt/homebrew")
    (add-to-list 'exec-path "/opt/homebrew/bin")
    (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH"))))
  
  ;; Intel Mac 的 Homebrew 路径
  (when (file-directory-p "/usr/local/bin")
    (add-to-list 'exec-path "/usr/local/bin"))
  
  ;; ==================== 性能优化 ====================
  (setq mac-allow-anti-aliasing t
        frame-resize-pixelwise t
        ns-use-proxy-icon nil
        ns-use-mwheel-acceleration nil
        ns-use-native-fullscreen t)
  
  (message "✓ macOS 专属配置已加载"))

(provide 'macos)
;;; macos.el ends here