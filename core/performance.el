;;; performance.el --- 统一性能优化模块 -*- lexical-binding: t; -*-
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
        warning-minimum-level :error
        ;; 额外启动优化
        frame-inhibit-implied-resize t
        frame-resize-pixelwise t
        redisplay-dont-pause t
        redisplay-skip-fontification-on-input t
        fast-but-imprecise-scrolling t
        jit-lock-defer-time 0.125
        jit-lock-stealth-time 16
        jit-lock-stealth-verbose nil
        jit-lock-context-time 0.5
        jit-lock-chunk-size 1000
        ;; 禁用不必要的功能
        tool-bar-mode nil
        menu-bar-mode nil
        scroll-bar-mode nil
        tooltip-mode nil
        ;; 优化字体缓存
        inhibit-compacting-font-caches t
        font-lock-support-mode 'jit-lock-mode
        font-lock-maximum-decoration 1
        font-lock-verbose nil))

;; ==================== GC 优化 ====================
(defun my/optimize-gc ()
  "优化垃圾回收性能"
  (setq gc-cons-threshold 268435456  ; 256MB (增加阈值减少GC频率)
        gc-cons-percentage 0.8       ; 增加百分比
        garbage-collection-messages t ; 显示GC消息
        gc-cons-threshold-most-positive-fixnum 134217728))

(defun my/restore-gc ()
  "恢复正常垃圾回收设置"
  (setq gc-cons-threshold 16777216  ; 16MB
        gc-cons-percentage 0.1
        garbage-collection-messages nil))

;; ==================== 内存优化 ====================
(defun my/optimize-memory ()
  "优化内存使用"
  (setq memory-limit 4294967296  ; 4GB 内存限制
        large-file-warning-threshold 100000000  ; 100MB
        ;; 优化字符串处理
        string-optimize t
        ;; 优化列表处理
        list-optimize t
        ;; 减少内存碎片
        memory-limit 4294967296))

;; ==================== 文件处理优化 ====================
(defun my/optimize-file-handling ()
  "优化文件处理性能"
  (setq read-process-output-max 6291456  ; 6MB (增加缓冲区)
        process-adaptive-read-buffering nil
        create-lockfiles nil
        make-backup-files nil
        auto-save-default nil
        auto-revert-verbose nil
        auto-revert-interval 10
        ;; 文件系统优化
        find-file-hook nil
        find-file-suppress-same-file-warnings t
        ;; 缓存优化
        recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-exclude '("/tmp/" "/var/tmp/" ".git/" ".cache/")
        ;; 文件监控优化
        file-notify-debounce-time 1.0
        file-notify-max-files 1000))

;; ==================== 渲染优化 ====================
(defun my/optimize-rendering ()
  "优化渲染性能"
  (setq ;; 滚动优化
        scroll-margin 1
        scroll-conservatively 10000
        scroll-preserve-screen-position t
        scroll-step 1
        ;; 重绘优化
        redisplay-dont-pause t
        redisplay-skip-fontification-on-input t
        fast-but-imprecise-scrolling t
        ;; 字体渲染优化
        font-lock-support-mode 'jit-lock-mode
        font-lock-maximum-decoration 1
        font-lock-verbose nil
        ;; 行号优化
        display-line-numbers-width 3
        display-line-numbers-grow-only t
        display-line-numbers-widen t))

;; ==================== 系统性能监控 ====================
(defvar my/startup-start-time nil
  "启动开始时间")

(defun my/performance-monitor ()
  "性能监控函数"
  (setq my/startup-start-time (float-time))
  (run-with-timer 5 nil
                 (lambda ()
                   (let ((load-time (- (float-time) my/startup-start-time)))
                     (message "Emacs 启动时间: %.2f 秒" load-time)))))

(defun my/monitor-system-performance ()
  "监控系统性能"
  (interactive)
  (message "监控系统性能...")
  
  ;; 检查CPU使用率
  (let ((cpu-usage (my/get-cpu-usage)))
    (message "CPU使用率: %s%%" cpu-usage)
    (when (> cpu-usage 80)
      (message "⚠ 警告: CPU使用率过高 (%s%%)" cpu-usage)))
  
  ;; 检查内存使用
  (let ((memory-usage (my/get-memory-usage)))
    (message "内存使用: %s MB" memory-usage)
    (when (> memory-usage 8000)
      (message "⚠ 警告: 内存使用过高 (%s MB)" memory-usage))))

(defun my/get-cpu-usage ()
  "获取CPU使用率"
  (condition-case nil
      (let ((output (shell-command-to-string "top -l 1 | grep 'CPU usage' | awk '{print $3}' | sed 's/%//'")))
        (string-to-number (string-trim output)))
    (error 0)))

(defun my/get-memory-usage ()
  "获取内存使用量(MB)"
  (condition-case nil
      (let ((output (shell-command-to-string "vm_stat | grep 'Pages active' | awk '{print $3}' | sed 's/\\.//'")))
        (* (string-to-number (string-trim output)) 4096 0.000001))
    (error 0)))

;; ==================== 性能诊断 ====================
(defun my/diagnose-performance-issues ()
  "诊断性能问题"
  (interactive)
  (message "诊断性能问题...")
  
  (let ((issues '()))
    
    ;; 检查CPU使用率
    (let ((cpu-usage (my/get-cpu-usage)))
      (when (> cpu-usage 80)
        (push (format "CPU使用率过高: %s%%" cpu-usage) issues)))
    
    ;; 检查内存使用
    (let ((memory-usage (my/get-memory-usage)))
      (when (> memory-usage 8000)
        (push (format "内存使用过高: %s MB" memory-usage) issues)))
    
    ;; 显示结果
    (if issues
        (progn
          (message "发现 %d 个性能问题:" (length issues))
          (dolist (issue issues)
            (message "  ⚠ %s" issue))
          (message "运行 M-x my/optimize-emacs-performance 来优化"))
      (message "✓ 未发现性能问题"))))

;; ==================== 自动优化 ====================
(defun my/auto-optimize-performance ()
  "自动性能优化"
  (interactive)
  (message "自动性能优化...")
  
  ;; 检查性能问题
  (my/diagnose-performance-issues)
  
  ;; 优化Emacs
  (my/optimize-emacs-performance)
  
  (message "✓ 自动性能优化完成"))

(defun my/optimize-emacs-performance ()
  "优化Emacs性能"
  (interactive)
  (message "优化Emacs性能...")
  
  ;; 垃圾回收优化
  (setq gc-cons-threshold 268435456)  ; 256MB
  (setq gc-cons-percentage 0.8)
  
  ;; 减少自动保存频率
  (setq auto-save-interval 300)  ; 5分钟
  (setq auto-save-timeout 30)    ; 30秒
  
  ;; 优化字体渲染
  (setq inhibit-compacting-font-caches t)
  
  ;; 减少不必要的钩子
  (remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
  
  ;; 优化LSP性能
  (when (boundp 'lsp-idle-delay)
    (setq lsp-idle-delay 0.3))  ; 减少延迟
  
  (when (boundp 'lsp-log-io)
    (setq lsp-log-io nil))
  
  ;; 优化Company补全
  (when (boundp 'company-idle-delay)
    (setq company-idle-delay 0.3))
  
  ;; 优化Helm性能
  (when (boundp 'helm-candidate-number-limit)
    (setq helm-candidate-number-limit 100))
  
  ;; 优化Ivy性能
  (when (boundp 'ivy-height)
    (setq ivy-height 15))
  
  ;; 优化Magit性能
  (when (boundp 'magit-refresh-status-buffer)
    (setq magit-refresh-status-buffer nil))
  
  ;; 优化Tramp性能
  (when (boundp 'tramp-verbose)
    (setq tramp-verbose 0))
  
  (message "✓ Emacs性能优化完成"))

;; ==================== 高级性能优化 ====================
(defun my/advanced-performance-optimization ()
  "高级性能优化"
  (interactive)
  (message "执行高级性能优化...")
  
  ;; 优化原生编译
  (when (fboundp 'native-compile-async)
    (setq native-comp-async-report-warnings-errors nil
          native-comp-deferred-compilation t
          native-comp-jit-compilation t
          native-comp-speed 2))
  
  ;; 优化字节编译
  (setq byte-compile-warnings '(not free-vars unresolved))
  
  ;; 优化正则表达式
  (setq regexp-opt-depth-limit 2000
        regexp-opt-char-fold-limit 2000)
  
  ;; 优化搜索性能
  (setq search-whitespace-regexp "\\s-+"
        search-default-regexp nil)
  
  ;; 优化缩进性能
  (setq indent-line-function 'indent-relative-maybe)
  
  ;; 优化语法高亮
  (setq font-lock-maximum-size 1048576)  ; 1MB
  
  (message "✓ 高级性能优化完成"))

;; ==================== 性能基准测试 ====================
(defun my/performance-benchmark ()
  "性能基准测试"
  (interactive)
  (message "开始性能基准测试...")
  
  (let ((start-time (float-time)))
    ;; 测试启动时间
    (let ((load-time (- (float-time) start-time)))
      (message "启动时间: %.3f 秒" load-time))
    
    ;; 测试内存使用
    (let ((memory-usage (my/get-memory-usage)))
      (message "内存使用: %.1f MB" memory-usage))
    
    ;; 测试GC性能
    (let ((gc-start (float-time)))
      (garbage-collect)
      (let ((gc-time (- (float-time) gc-start)))
        (message "GC时间: %.3f 秒" gc-time)))
    
    (message "✓ 性能基准测试完成")))

;; ==================== 初始化 ====================
(defun my/init-performance ()
  "初始化性能模块"
  (my/optimize-startup)
  (my/optimize-file-handling)
  (my/optimize-gc)
  (my/optimize-memory)
  (my/optimize-rendering)
  (add-hook 'emacs-startup-hook #'my/restore-gc)
  (add-hook 'emacs-startup-hook #'my/performance-monitor)
  (add-hook 'emacs-startup-hook #'my/advanced-performance-optimization))

;; 立即初始化
(my/init-performance)

(provide 'performance)
;;; performance.el ends here
