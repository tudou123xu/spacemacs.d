;;; integration.el --- 系统集成模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024
;; Description: 提供跨平台的系统集成功能，包括环境变量、编码、文件操作等

;; ==================== 环境变量处理 ====================
(defun my/setup-environment ()
  "设置环境变量，支持跨平台"
  (when (or (my/system-is-mac) (my/system-is-linux))
    (when (and (package-installed-p 'exec-path-from-shell)
               (display-graphic-p))
      (condition-case err
          (progn
            (setq exec-path-from-shell-variables 
                  '("PATH" "GOPATH" "GOROOT" "PYTHONPATH" "NODE_PATH" "JAVA_HOME")
                  exec-path-from-shell-check-startup-files nil
                  exec-path-from-shell-arguments '("-l"))
            (exec-path-from-shell-initialize)
            (message "✓ 环境变量已初始化"))
        (error
         (message "✗ 环境变量初始化失败: %s" (error-message-string err)))))))

;; ==================== 编码设置 ====================
(defun my/setup-encoding ()
  "设置统一的编码系统"
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8
        locale-coding-system 'utf-8)
  
  ;; Windows 特殊处理
  (when (my/system-is-windows)
    (setq file-name-coding-system 'gbk
          default-process-coding-system '(utf-8 . gbk)))
  
  (message "✓ 编码设置已应用"))

;; ==================== 文件操作优化 ====================
(defun my/setup-file-operations ()
  "优化文件操作性能和行为"
  (setq large-file-warning-threshold (* 100 1024 1024)  ; 100MB
        vc-handled-backends '(Git)
        vc-follow-symlinks t
        find-file-visit-truename t
        delete-by-moving-to-trash t
        ;; 自动刷新文件
        auto-revert-verbose nil
        auto-revert-interval 5
        global-auto-revert-non-file-buffers t)
  
  ;; 启用全局自动刷新
  (global-auto-revert-mode 1)
  
  (message "✓ 文件操作优化已应用"))

;; ==================== 备份和自动保存 ====================
(defun my/setup-backup-settings ()
  "配置备份和自动保存策略"
  (let ((backup-dir (expand-file-name "backups/" user-emacs-directory))
        (auto-save-dir (expand-file-name "auto-saves/" user-emacs-directory)))
    
    ;; 确保目录存在
    (dolist (dir (list backup-dir auto-save-dir))
      (unless (file-exists-p dir)
        (make-directory dir t)))
    
    ;; 配置备份
    (setq backup-directory-alist `(("." . ,backup-dir))
          backup-by-copying t
          delete-old-versions t
          kept-new-versions 6
          kept-old-versions 2
          version-control t)
    
    ;; 配置自动保存
    (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))
          auto-save-default t
          auto-save-interval 200
          auto-save-timeout 20)
    
    (message "✓ 备份和自动保存已配置")))

;; ==================== 初始化 ====================
(defun my/init-system-integration ()
  "初始化系统集成模块"
  (message "初始化系统集成模块...")
  
  ;; 设置环境变量
  (my/setup-environment)
  
  ;; 设置编码
  (my/setup-encoding)
  
  ;; 优化文件操作
  (my/setup-file-operations)
  
  ;; 配置备份设置
  (my/setup-backup-settings)
  
  ;; 加载平台特定配置
  (my/load-platform-specific-config)
  
  (message "✓ 系统集成模块初始化完成"))

(defun my/load-platform-specific-config ()
  "加载平台特定的配置文件"
  (let ((platform-file
         (cond
          ((my/system-is-mac) 
           (expand-file-name "system/platform/macos.el" "~/.spacemacs.d/"))
          ((my/system-is-windows)
           (expand-file-name "system/platform/windows.el" "~/.spacemacs.d/"))
          ((my/system-is-linux)
           (expand-file-name "system/platform/linux.el" "~/.spacemacs.d/"))
          (t nil))))
    
    (when (and platform-file (file-exists-p platform-file))
      (condition-case err
          (progn
            (load-file platform-file)
            (message "✓ 加载平台配置: %s" (file-name-nondirectory platform-file)))
        (error
         (message "✗ 平台配置加载失败: %s" (error-message-string err)))))))

;; 延迟初始化（等待 common 模块加载）
(run-with-timer 1.5 nil #'my/init-system-integration)

(provide 'integration)
;;; integration.el ends here