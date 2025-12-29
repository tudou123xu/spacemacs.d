;;; ui-enhancement.el --- 界面增强模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024
;; Description: 提供界面美化和用户体验优化

;; ==================== 视觉优化 ====================
(defun my/setup-visual-enhancements ()
  "设置视觉增强效果"
  (when (display-graphic-p)
    ;; 启用视觉行模式
    (global-visual-line-mode 1)
    (setq truncate-lines nil
          word-wrap t)
    
    ;; 平滑滚动
    (setq scroll-conservatively 10000
          scroll-preserve-screen-position t
          scroll-margin 3)
    
    (message "✓ 视觉增强已启用")))

;; ==================== 字体配置 ====================
(defun my/setup-fonts ()
  "配置系统最佳字体"
  (when (display-graphic-p)
    (let ((font-size 14)
          (default-font
            (cond ((my/system-is-mac) "SF Mono")
                  ((my/system-is-windows) "Consolas")
                  (t "Source Code Pro"))))
      
      ;; 设置默认字体
      (condition-case err
          (progn
            (set-face-attribute 'default nil 
                               :font (format "%s-%d" default-font font-size))
            (message "✓ 字体已设置: %s %d" default-font font-size))
        (error
         (message "⚠ 字体设置失败: %s，使用默认字体" 
                  (error-message-string err)))))))

;; ==================== 主题配置 ====================
(defun my/setup-theme ()
  "配置编辑器主题和外观"
  ;; 高亮当前行
  (global-hl-line-mode 1)
  
  ;; 显示行号（Emacs 26.1+）
  (when (version<= "26.1" emacs-version)
    (global-display-line-numbers-mode 1)
    (setq display-line-numbers-type 'relative
          display-line-numbers-width 3
          display-line-numbers-grow-only t))
  
  ;; 括号匹配高亮
  (show-paren-mode 1)
  (setq show-paren-delay 0
        show-paren-style 'parenthesis)
  
  (message "✓ 主题配置已应用"))

;; ==================== 窗口管理 ====================
(defun my/setup-window-management ()
  "配置窗口管理行为"
  (setq split-width-threshold 160
        split-height-threshold 80)
  
  ;; 窗口切换增强
  (when (fboundp 'winner-mode)
    (winner-mode 1))
  
  (message "✓ 窗口管理已配置"))

;; ==================== 初始化 ====================
(defun my/init-ui-enhancement ()
  "初始化界面增强模块"
  (message "初始化界面增强模块...")
  
  (my/setup-visual-enhancements)
  (my/setup-fonts)
  (my/setup-theme)
  (my/setup-window-management)
  
  (message "✓ 界面增强模块初始化完成"))

;; 延迟初始化（等待基础模块加载）
(run-with-timer 2 nil #'my/init-ui-enhancement)

(provide 'ui-enhancement)
;;; ui-enhancement.el ends here