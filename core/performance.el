;;; performance.el --- 性能优化核心模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 启动优化 ====================
(defun my/optimize-startup ()
  "优化 Emacs 启动速度"
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil
        inhibit-splash-screen t
        package-enable-at-startup nil
        package-quickstart nil
        package--init-file-ensured t
        inhibit-compacting-font-caches t
        font-lock-maximum-decoration 1
        font-lock-verbose nil
        file-name-handler-alist nil
        vc-handled-backends nil
        auto-save-default nil
        make-backup-files nil
        message-log-max 1000
        warning-minimum-level :error))

;; ==================== GC 优化 ====================
(defun my/optimize-gc ()
  "优化垃圾回收性能"
  (setq gc-cons-threshold 134217728  ; 128MB
        gc-cons-percentage 0.6))

(defun my/restore-gc ()
  "恢复正常垃圾回收设置"
  (setq gc-cons-threshold 16777216  ; 16MB
        gc-cons-percentage 0.1))

;; ==================== 文件处理优化 ====================
(defun my/optimize-file-handling ()
  "优化文件处理性能"
  (setq read-process-output-max 3145728  ; 3MB
        process-adaptive-read-buffering nil
        create-lockfiles nil
        make-backup-files nil
        auto-save-default nil
        auto-revert-verbose nil
        auto-revert-interval 10))

;; ==================== 性能监控 ====================
(defvar my/startup-start-time nil
  "启动开始时间")

(defun my/performance-monitor ()
  "性能监控函数"
  (setq my/startup-start-time (float-time))
  (run-with-timer 5 nil
                 (lambda ()
                   (let ((load-time (- (float-time) my/startup-start-time)))
                     (message "Emacs 启动时间: %.2f 秒" load-time)))))

;; ==================== 初始化 ====================
(defun my/init-performance ()
  "初始化性能模块"
  (my/optimize-startup)
  (my/optimize-file-handling)
  (my/optimize-gc)
  (add-hook 'emacs-startup-hook #'my/restore-gc)
  (add-hook 'emacs-startup-hook #'my/performance-monitor))

;; 立即初始化
(my/init-performance)

(provide 'performance)
;;; performance.el ends here
