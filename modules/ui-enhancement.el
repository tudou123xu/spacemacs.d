;;; ui-enhancement.el --- 界面增强模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 视觉优化 ====================
(defun my/setup-visual-enhancements ()
  "设置视觉增强"
  (when (display-graphic-p)
    ;; 视觉行模式
    (global-visual-line-mode 1)
    (setq truncate-lines nil)
    
    ;; 平滑滚动
    (setq scroll-conservatively 10000
          scroll-preserve-screen-position t
          scroll-margin 3)))

;; ==================== 字体配置 ====================
(defun my/setup-fonts ()
  "配置字体"
  (when (display-graphic-p)
    (let ((font-size 14)
          (default-font 
            (cond ((my/system-is-mac) "SF Mono")
                  ((my/system-is-windows) "Consolas")
                  (t "Source Code Pro"))))
      
      ;; 设置默认字体
      (condition-case nil
          (set-face-attribute 'default nil :font (format "%s-%d" default-font font-size))
        (error (message "字体设置失败，使用默认字体"))))))

;; ==================== 主题配置 ====================
(defun my/setup-theme ()
  "配置主题"
  ;; 高亮当前行
  (global-hl-line-mode 1)
  
  ;; 显示行号
  (when (version<= "26.1" emacs-version)
    (global-display-line-numbers-mode 1)
    (setq display-line-numbers-type 'relative))
  
  ;; 括号匹配
  (show-paren-mode 1)
  (setq show-paren-delay 0
        show-paren-style 'parenthesis))

;; ==================== 初始化 ====================
(defun my/init-ui-enhancement ()
  "初始化界面增强模块"
  (message "初始化界面增强模块...")
  
  (my/setup-visual-enhancements)
  (my/setup-fonts)
  (my/setup-theme)
  
  (message "界面增强模块初始化完成"))

;; 延迟初始化
(run-with-timer 2 nil #'my/init-ui-enhancement)

(provide 'ui-enhancement)
;;; ui-enhancement.el ends here