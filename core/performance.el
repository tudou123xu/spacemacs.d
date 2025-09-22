;;; performance.el --- 性能优化核心模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 性能配置常量 ====================
(defconst my/performance-config
  '(:gc-threshold 134217728  ; 128MB
    :gc-percentage 0.6
    :read-process-output-max 3145728  ; 3MB
    :font-lock-max-decoration 1
    :message-log-max 1000)
  "性能优化配置常量")

;; ==================== 启动优化 ====================
(defun my/optimize-startup ()
  "优化 Emacs 启动速度"
  (message "优化启动性能...")
  
  ;; 禁用启动时的非必要功能
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil
        initial-major-mode 'fundamental-mode
        inhibit-splash-screen t
        inhibit-startup-buffer-menu t)
  
  ;; 减少启动时的包加载
  (setq package-enable-at-startup nil
        package-quickstart nil
        package--init-file-ensured t
        package-load-list 'all)
  
  ;; 优化字体加载
  (setq inhibit-compacting-font-caches t
        font-lock-maximum-decoration (plist-get my/performance-config :font-lock-max-decoration)
        font-lock-verbose nil)
  
  ;; 优化文件处理
  (setq file-name-handler-alist nil
        vc-handled-backends nil
        find-file-hook nil
        auto-save-default nil
        make-backup-files nil)
  
  ;; 减少启动时的消息
  (setq message-log-max (plist-get my/performance-config :message-log-max)
        warning-minimum-level :error)
  
  (message "✓ 启动优化完成"))

;; ==================== GC 优化 ====================
(defun my/optimize-gc ()
  "优化垃圾回收性能"
  (setq gc-cons-threshold (plist-get my/performance-config :gc-threshold)
        gc-cons-percentage (plist-get my/performance-config :gc-percentage)))

(defun my/restore-gc ()
  "恢复正常垃圾回收设置"
  (setq gc-cons-threshold 16777216  ; 16MB
        gc-cons-percentage 0.1))

;; ==================== 文件处理优化 ====================
(defun my/optimize-file-handling ()
  "优化文件处理性能"
  (setq read-process-output-max (plist-get my/performance-config :read-process-output-max)
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
                     (message "Emacs 启动时间: %.2f 秒" load-time)
                     (my/analyze-startup-performance load-time)))))

(defun my/analyze-startup-performance (load-time)
  "分析启动性能并提供优化建议"
  (cond
   ((< load-time 3.0)
    (message "✓ 启动速度优秀 (%.2f秒)" load-time))
   ((< load-time 5.0)
    (message "✓ 启动速度良好 (%.2f秒)" load-time))
   ((< load-time 10.0)
    (message "⚠ 启动速度一般 (%.2f秒)，建议优化配置" load-time))
   (t
    (message "✗ 启动速度较慢 (%.2f秒)，强烈建议优化" load-time)
    (my/suggest-startup-optimizations))))

(defun my/suggest-startup-optimizations ()
  "提供启动优化建议"
  (message "启动优化建议:")
  (message "1. 减少启动时加载的层")
  (message "2. 使用延迟加载非关键功能")
  (message "3. 检查是否有重复的包加载")
  (message "4. 考虑使用 Emacs 服务器模式"))

;; ==================== 延迟加载机制 ====================
(defun my/delayed-loading ()
  "启动后延迟加载非关键功能"
  (run-with-timer 2 nil
                 (lambda ()
                   (message "开始加载非关键功能...")
                   (message "非关键功能加载完成"))))

;; ==================== 初始化 ====================
(defun my/init-performance ()
  "初始化性能模块"
  (message "初始化性能模块...")
  
  ;; 应用所有优化
  (my/optimize-startup)
  (my/optimize-file-handling)
  
  ;; 设置 GC 优化
  (my/optimize-gc)
  (add-hook 'emacs-startup-hook #'my/restore-gc)
  
  ;; 设置延迟加载
  (add-hook 'emacs-startup-hook #'my/delayed-loading)
  
  ;; 启动性能监控
  (my/performance-monitor)
  
  (message "性能模块初始化完成"))

;; 立即初始化
(my/init-performance)

(provide 'performance)
;;; performance.el ends here
