;;; ui-enhancement.el --- 界面增强模块 -*- lexical-binding: t; -*-

;; ==================== 视觉优化 ====================
(defun my/enable-smart-visuals ()
  "启用智能视觉效果"
  (when (display-graphic-p)
    (global-visual-line-mode 1)
    (setq truncate-lines nil)
    (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
    
    ;; 平滑滚动
    (setq scroll-conservatively 10000
          scroll-preserve-screen-position t
          scroll-margin 3)))

(add-hook 'after-init-hook #'my/enable-smart-visuals)

;; ==================== 字体配置优化 ====================
(defun my/setup-fonts ()
  "跨平台字体配置"
  (when (display-graphic-p)
    (let* ((font-size 14)
           (default-font 
             (cond ((my/system-is-mac) 
                    (if (member "SF Mono" (font-family-list))
                        (format "SF Mono-%d" font-size)
                      (format "Monaco-%d" font-size)))
                   ((my/system-is-windows)
                    (format "Consolas-%d" font-size))
                   (t (format "Source Code Pro-%d" font-size)))))
      
      ;; 设置默认字体
      (condition-case nil
          (set-face-attribute 'default nil :font default-font)
        (error (message "字体设置失败，使用默认字体")))
      
      ;; 中文字体配置
      (when (my/system-is-mac)
        (dolist (charset '(han cjk-misc))
          (condition-case nil
              (set-fontset-font t charset "PingFang SC")
            (error nil))))
      
      (when (my/system-is-windows)
        (dolist (charset '(han cjk-misc))
          (condition-case nil
              (set-fontset-font t charset "Microsoft YaHei UI")
            (error nil)))))))

(add-hook 'after-init-hook #'my/setup-fonts)

;; ==================== 主题优化 ====================
(defun my/theme-enhancements ()
  "主题增强设置"
  ;; 高亮当前行
  (global-hl-line-mode 1)
  
  ;; 显示行号优化
  (when (version<= "26.1" emacs-version)
    (global-display-line-numbers-mode 1)
    (setq display-line-numbers-type 'relative))
  
  ;; 括号匹配增强
  (show-paren-mode 1)
  (setq show-paren-delay 0
        show-paren-style 'parenthesis))

(add-hook 'after-init-hook #'my/theme-enhancements)

(provide 'ui-enhancement)
;;; ui-enhancement.el ends here 