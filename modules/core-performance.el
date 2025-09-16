;;; core-performance.el --- 核心性能优化模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 启动速度优化 ====================
(defun my/optimize-startup ()
  "优化 Emacs 启动速度"
  ;; 禁用启动时的非必要功能
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil
        initial-major-mode 'fundamental-mode)
  
  ;; 减少启动时的包加载
  (setq package-enable-at-startup nil
        package-quickstart nil
        package--init-file-ensured t))

;; ==================== 系统检测函数 ====================
(defun my/system-is-mac ()
  "检测是否为 macOS 系统"
  (eq system-type 'darwin))

(defun my/system-is-linux ()
  "检测是否为 Linux 系统"
  (eq system-type 'gnu/linux))

(defun my/system-is-windows ()
  "检测是否为 Windows 系统"
  (eq system-type 'windows-nt))

(defun my/number-of-processors ()
  "动态获取处理器核心数（跨平台兼容）"
  (let ((default-threads 4))
    (or (ignore-errors
          (cond
           ((and (my/system-is-mac) (executable-find "/usr/sbin/sysctl"))
            (string-to-number (shell-command-to-string "sysctl -n hw.logicalcpu")))
           ((executable-find "nproc")
            (string-to-number (shell-command-to-string "nproc")))
           ((my/system-is-windows)
            (string-to-number (or (getenv "NUMBER_OF_PROCESSORS") "4")))))
        default-threads)))

;; ==================== GC 优化 ====================
(defun my/optimize-gc ()
  "优化垃圾回收性能"
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6))

(defun my/restore-gc ()
  "恢复正常垃圾回收设置"
  (setq gc-cons-threshold 16777216  ; 16MB
        gc-cons-percentage 0.1))

;; ==================== 包管理优化 ====================
(defun my/setup-package-archives ()
  "配置优化的包源"
  (setq package-archives
        '(("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ("gnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ("nongnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
          ("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/"))
        package-check-signature 'allow-unsigned
        package-enable-at-startup nil))

;; ==================== 文件处理优化 ====================
(defun my/optimize-file-handling ()
  "优化文件处理性能"
  (setq read-process-output-max (* 3 1024 1024)  ; 3MB for LSP
        process-adaptive-read-buffering nil
        create-lockfiles nil                      ; 避免锁文件
        make-backup-files nil                     ; 禁用备份文件
        auto-save-default nil                     ; 禁用自动保存
        auto-revert-verbose nil                   ; 减少自动刷新提示
        auto-revert-interval 10))                 ; 增加自动刷新间隔

;; ==================== 字体缓存优化 ====================
(defun my/optimize-font-cache ()
  "优化字体缓存"
  (setq inhibit-compacting-font-caches t          ; 避免字体缓存压缩
        font-lock-maximum-decoration 1))          ; 减少语法高亮装饰

;; ==================== 延迟加载机制 ====================
(defun my/delayed-loading ()
  "启动后延迟加载非关键功能"
  (run-with-timer 2 nil #'my/load-non-critical-features))

(defun my/load-non-critical-features ()
  "加载非关键功能"
  (message "开始加载非关键功能...")
  
  ;; 延迟加载语言支持
  (when (fboundp 'my/load-config-module)
    (my/load-config-module "lang-support")
    (my/load-config-module "ui-enhancement"))
  
  (message "非关键功能加载完成"))

;; ==================== 性能监控 ====================
(defun my/performance-monitor ()
  "性能监控函数"
  (let ((start-time (float-time)))
    (run-with-timer 5 nil
                   (lambda ()
                     (let ((load-time (- (float-time) start-time)))
                       (message "Emacs 启动时间: %.2f 秒" load-time))))))

;; ==================== 初始化 ====================
(defun my/init-core-performance ()
  "初始化核心性能模块"
  (message "初始化核心性能模块...")
  
  ;; 应用所有优化
  (my/optimize-startup)
  (my/setup-package-archives)
  (my/optimize-file-handling)
  (my/optimize-font-cache)
  
  ;; 设置 GC 优化
  (my/optimize-gc)
  (add-hook 'emacs-startup-hook #'my/restore-gc)
  
  ;; 设置延迟加载
  (add-hook 'emacs-startup-hook #'my/delayed-loading)
  
  ;; 启动性能监控
  (my/performance-monitor)
  
  (message "核心性能模块初始化完成"))

;; 立即初始化
(my/init-core-performance)

(provide 'core-performance)
;;; core-performance.el ends here