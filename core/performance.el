;;; performance.el --- 统一性能优化模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024
;; Description: 提供全面的性能优化、监控和诊断功能

;; ==================== 常量定义 ====================
(defconst my/gc-cons-threshold-normal 16777216
  "正常 GC 阈值 (16MB)")

(defconst my/gc-cons-threshold-high 268435456
  "高性能 GC 阈值 (256MB)")

(defconst my/gc-cons-threshold-startup 134217728
  "启动时 GC 阈值 (128MB)")

(defconst my/large-file-threshold (* 100 1024 1024)
  "大文件警告阈值 (100MB)")

;; ==================== 变量定义 ====================
(defvar my/startup-start-time nil
  "启动开始时间")

(defvar my/performance-mode 'normal
  "性能模式: normal, high-performance, conservative")

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
(defun my/optimize-gc (&optional mode)
  "优化垃圾回收性能
参数:
  MODE - 性能模式: normal, high-performance, conservative"
  (let ((mode (or mode my/performance-mode)))
    (pcase mode
      ('high-performance
       (setq gc-cons-threshold my/gc-cons-threshold-high
             gc-cons-percentage 0.8)
       (message "GC 设置为高性能模式: %dMB" (/ my/gc-cons-threshold-high 1048576)))
      ('conservative
       (setq gc-cons-threshold my/gc-cons-threshold-normal
             gc-cons-percentage 0.1)
       (message "GC 设置为保守模式: %dMB" (/ my/gc-cons-threshold-normal 1048576)))
      (_
       (setq gc-cons-threshold my/gc-cons-threshold-startup
             gc-cons-percentage 0.5)
       (message "GC 设置为正常模式: %dMB" (/ my/gc-cons-threshold-startup 1048576))))
    
    ;; 设置 GC 消息显示
    (setq garbage-collection-messages (eq mode 'conservative))))

(defun my/restore-gc ()
  "恢复正常垃圾回收设置"
  (setq gc-cons-threshold my/gc-cons-threshold-normal
        gc-cons-percentage 0.1
        garbage-collection-messages nil)
  (message "GC 已恢复到正常设置: %dMB" (/ my/gc-cons-threshold-normal 1048576)))

;; ==================== 内存优化 ====================
(defun my/optimize-memory ()
  "优化内存使用策略"
  (when (>= emacs-major-version 27)
    (setq memory-limit 4294967296          ; 4GB 内存限制
          large-file-warning-threshold my/large-file-threshold
          warning-minimum-level :error))   ; 减少警告输出
  
  ;; 优化字符串和列表处理（仅在支持的版本中）
  (when (boundp 'string-chasing-threshold)
    (setq string-chasing-threshold (* 8 1024 1024)))  ; 8MB
  
  (message "✓ 内存优化已应用"))

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
(defun my/performance-monitor ()
  "性能监控函数，记录启动时间"
  (setq my/startup-start-time (float-time))
  (run-with-timer 5 nil
                 (lambda ()
                   (let ((load-time (- (float-time) my/startup-start-time)))
                     (message "✓ Emacs 启动时间: %.2f 秒" load-time)
                     (when (require 'error-handling nil t)
                       (my/log-info "Emacs 启动完成" 
                                   (format "时间: %.2f 秒" load-time)))))))

(defun my/get-system-info ()
  "获取系统信息"
  (list :emacs-version emacs-version
        :system-type system-type
        :system-name (system-name)
        :gc-threshold gc-cons-threshold
        :features-count (length features)
        :process-output-max read-process-output-max))

(defun my/display-system-info ()
  "显示系统信息"
  (interactive)
  (let ((info (my/get-system-info)))
    (message "系统信息: Emacs %s, %s, GC: %dMB, 已加载特性: %d"
             (plist-get info :emacs-version)
             (plist-get info :system-type)
             (/ (plist-get info :gc-threshold) 1048576)
             (plist-get info :features-count))))

;; ==================== 性能诊断 ====================
(defun my/diagnose-performance ()
  "诊断性能问题并给出建议"
  (interactive)
  (message "诊断性能问题...")
  
  (let ((issues '())
        (suggestions '()))
    
    ;; 检查 GC 设置
    (when (< gc-cons-threshold 8000000)
      (push "GC 阈值过低，可能导致频繁垃圾回收" issues)
      (push "运行 (my/optimize-gc 'high-performance) 提升性能" suggestions))
    
    ;; 检查进程输出缓冲区
    (when (< read-process-output-max 1048576)
      (push "进程输出缓冲区较小，可能影响 LSP 性能" issues)
      (push "增加 read-process-output-max 到至少 1MB" suggestions))
    
    ;; 检查已加载特性数量
    (when (> (length features) 500)
      (push (format "已加载 %d 个特性，可能影响启动速度" (length features)) issues)
      (push "考虑使用延迟加载或减少不必要的包" suggestions))
    
    ;; 显示结果
    (if issues
        (progn
          (message "发现 %d 个潜在问题:" (length issues))
          (dolist (issue issues)
            (message "  ⚠ %s" issue))
          (message "\n建议:")
          (dolist (suggestion suggestions)
            (message "  → %s" suggestion)))
      (message "✓ 未发现明显性能问题"))))

;; ==================== 自动优化 ====================
(defun my/optimize-emacs-performance ()
  "一键优化 Emacs 性能"
  (interactive)
  (message "开始优化 Emacs 性能...")
  
  ;; 优化 GC
  (my/optimize-gc 'high-performance)
  
  ;; 优化文件处理
  (setq read-process-output-max 6291456  ; 6MB
        create-lockfiles nil
        make-backup-files nil
        auto-save-default nil)
  
  ;; 优化 LSP 性能
  (when (boundp 'lsp-idle-delay)
    (setq lsp-idle-delay 0.3
          lsp-log-io nil))
  
  ;; 优化 Company 补全
  (when (boundp 'company-idle-delay)
    (setq company-idle-delay 0.2))
  
  ;; 优化 Tramp 性能
  (when (boundp 'tramp-verbose)
    (setq tramp-verbose 1))
  
  (message "✓ Emacs 性能优化完成")
  (my/diagnose-performance))

;; ==================== 高级性能优化 ====================
(defun my/advanced-performance-optimization ()
  "高级性能优化，针对原生编译等高级特性"
  (interactive)
  (message "执行高级性能优化...")
  
  ;; 优化原生编译（Emacs 28+）
  (when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
    (setq native-comp-async-report-warnings-errors nil
          native-comp-deferred-compilation t
          native-comp-speed 2)
    (message "✓ 原生编译优化已启用"))
  
  ;; 优化字节编译
  (setq byte-compile-warnings '(not free-vars unresolved noruntime))
  
  ;; 优化正则表达式
  (when (boundp 'regexp-opt-depth-limit)
    (setq regexp-opt-depth-limit 2000))
  
  ;; 优化语法高亮
  (setq font-lock-maximum-size 1048576  ; 1MB
        font-lock-support-mode 'jit-lock-mode)
  
  (message "✓ 高级性能优化完成"))

;; ==================== 性能基准测试 ====================
(defun my/performance-benchmark ()
  "性能基准测试"
  (interactive)
  (message "开始性能基准测试...")
  
  (let ((results '()))
    ;; 测试启动时间
    (when my/startup-start-time
      (let ((startup-time (- (float-time) my/startup-start-time)))
        (push (format "启动时间: %.3f 秒" startup-time) results)))
    
    ;; 测试 GC 性能
    (let ((gc-start (float-time)))
      (garbage-collect)
      (let ((gc-time (- (float-time) gc-start)))
        (push (format "GC 时间: %.3f 秒" gc-time) results)))
    
    ;; 显示系统信息
    (let ((info (my/get-system-info)))
      (push (format "GC 阈值: %dMB" (/ (plist-get info :gc-threshold) 1048576)) results)
      (push (format "已加载特性: %d" (plist-get info :features-count)) results))
    
    ;; 输出结果
    (message "\n========== 性能基准测试结果 ==========")
    (dolist (result (reverse results))
      (message "  %s" result))
    (message "=====================================\n")
    
    results))

;; ==================== 性能模式切换 ====================
(defun my/switch-performance-mode (mode)
  "切换性能模式
参数:
  MODE - 性能模式: normal, high-performance, conservative"
  (interactive
   (list (intern (completing-read "选择性能模式: "
                                   '("normal" "high-performance" "conservative")
                                   nil t))))
  (setq my/performance-mode mode)
  (my/optimize-gc mode)
  (message "✓ 已切换到 %s 性能模式" mode))

;; ==================== 初始化 ====================
(defun my/init-performance ()
  "初始化性能模块"
  (message "初始化性能模块...")
  
  ;; 应用启动优化
  (my/optimize-startup)
  
  ;; 应用文件处理优化
  (my/optimize-file-handling)
  
  ;; 应用 GC 优化（启动模式）
  (my/optimize-gc 'normal)
  
  ;; 应用内存优化
  (my/optimize-memory)
  
  ;; 应用渲染优化
  (my/optimize-rendering)
  
  ;; 启动后恢复正常 GC 设置
  (add-hook 'emacs-startup-hook #'my/restore-gc)
  
  ;; 启动性能监控
  (add-hook 'emacs-startup-hook #'my/performance-monitor)
  
  ;; 延迟执行高级优化
  (add-hook 'emacs-startup-hook #'my/advanced-performance-optimization)
  
  (message "✓ 性能模块初始化完成"))

;; 立即初始化
(my/init-performance)

(provide 'performance)
;;; performance.el ends here
