;;; macos-specific.el --- macOS 专属配置 -*- lexical-binding: t; -*-

;; ==================== macOS 系统集成 ====================
(when (spacemacs/system-is-mac)
  
  ;; 环境变量优化
  (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-check-startup-files nil)
  
  ;; 系统快捷键映射
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-function-modifier 'hyper)
  
  ;; ==================== 文件系统优化 ====================
  ;; 禁用 .DS_Store 文件创建
  (setq insert-directory-program "/usr/local/bin/gls"  ; 使用 GNU ls
        dired-use-ls-dired t)
  
  ;; Spotlight 集成
  (when (executable-find "mdfind")
    (setq locate-command "mdfind"))
  
  ;; ==================== 系统服务集成 ====================
  ;; 通知中心集成
  (defun macos/notify (title message)
    "发送 macOS 通知"
    (when (executable-find "osascript")
      (call-process "osascript" nil nil nil
                    "-e" (format "display notification \"%s\" with title \"%s\"" 
                                message title))))
  
  ;; Finder 集成
  (defun macos/reveal-in-finder ()
    "在 Finder 中显示当前文件"
    (interactive)
    (when buffer-file-name
      (call-process "open" nil nil nil "-R" buffer-file-name)))
  
  ;; ==================== 输入法优化 ====================
  ;; 中文输入法配置
  (use-package pyim
    :ensure t
    :config
    (setq pyim-default-scheme 'quanpin
          pyim-page-tooltip 'posframe
          pyim-page-length 9)
    
    ;; 启用模糊拼音
    (setq pyim-fuzzy-pinyin-alist
          '(("en" "eng")
            ("in" "ing")
            ("l" "n")
            ("z" "zh")
            ("c" "ch")
            ("s" "sh")))
    
    ;; 快捷键绑定
    (global-set-key (kbd "C-\\") 'toggle-input-method))
  
  ;; ==================== 系统级快捷键 ====================
  (global-set-key (kbd "s-f") #'spacemacs/helm-find-files)
  (global-set-key (kbd "s-r") #'macos/reveal-in-finder)
  (global-set-key (kbd "s-w") #'delete-window)
  (global-set-key (kbd "s-q") #'save-buffers-kill-terminal)
  
  ;; ==================== Homebrew 集成 ====================
  (when (file-directory-p "/opt/homebrew")
    (add-to-list 'exec-path "/opt/homebrew/bin")
    (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH"))))
  
  ;; ==================== 性能优化 ====================
  ;; 减少 macOS 特有的延迟
  (setq mac-allow-anti-aliasing t
        frame-resize-pixelwise t)
  
  ;; 禁用不必要的 macOS 功能
  (setq ns-use-proxy-icon nil
        ns-use-mwheel-acceleration nil)
  
  (message "macOS 专属配置已加载"))

(provide 'macos-specific)
;;; macos-specific.el ends here 