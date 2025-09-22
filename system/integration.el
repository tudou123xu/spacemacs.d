;;; system-integration.el --- 系统集成模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 环境变量处理 ====================
(defun my/setup-environment ()
  "设置环境变量"
  (when (or (my/system-is-mac) (my/system-is-linux))
    (use-package exec-path-from-shell
      :ensure t
      :config
      (progn
        (setq exec-path-from-shell-variables 
              '("PATH" "GOPATH" "GOROOT" "PYTHONPATH"))
        (exec-path-from-shell-initialize)))))

;; ==================== 编码设置 ====================
(defun my/setup-encoding ()
  "设置编码"
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; ==================== 文件操作优化 ====================
(defun my/setup-file-operations ()
  "优化文件操作"
  (setq large-file-warning-threshold (* 100 1024 1024)  ; 100MB
        vc-handled-backends '(Git)
        vc-follow-symlinks t))

;; ==================== 初始化 ====================
(defun my/init-system-integration ()
  "初始化系统集成模块"
  (message "初始化系统集成模块...")
  
  (my/setup-environment)
  (my/setup-encoding)
  (my/setup-file-operations)
  
  (message "系统集成模块初始化完成"))

;; 延迟初始化
(run-with-timer 1.5 nil #'my/init-system-integration)

(provide 'system-integration)
;;; system-integration.el ends here