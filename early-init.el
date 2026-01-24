;;; early-init.el --- Early Initialization for Spacemacs -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Description: Optimizations that must run before GUI startup

;; ==================== 环境变量设置 ====================
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; 确保 Homebrew 路径在 exec-path 中，否则 Native Comp 找不到 gcc
(when (eq system-type 'darwin)
  (let ((brew-bin "/opt/homebrew/bin"))
    (when (file-directory-p brew-bin)
      (add-to-list 'exec-path brew-bin)
      (setenv "PATH" (concat brew-bin ":" (getenv "PATH"))))))

;; ==================== 兼容性修复 ====================
;; 修复 "Symbol's value as variable is void: shell-enable-vterm-support"
(defvar shell-enable-vterm-support t "Enable vterm support for shell layer.")


;; ==================== 垃圾回收 (GC) 优化 ====================
;; 在启动阶段将 GC 阈值设置为极大值 (1GB)，避免启动过程中频繁 GC
(setq gc-cons-threshold (* 1024 1024 1024))
(setq gc-cons-percentage 0.6)

;; ==================== 包管理器优化 ====================
;; 禁止在启动时自动初始化包管理器 (由 Spacemacs 接管)
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

;; ==================== UI 闪烁抑制 ====================
;; 在 GUI 创建前禁用不必要的 UI 元素，防止启动时界面抖动
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; 禁用启动画面和消息
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; ==================== 文件名处理优化 ====================
;; 避免在 macOS 上处理文件名时的编码问题
(when (eq system-type 'darwin)
  (setq file-name-coding-system 'utf-8-hfs))

;; ==================== 原生编译优化 (Native Comp) ====================
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  
  ;; 设置原生编译缓存路径（在 user-emacs-directory 下）
  (setq native-comp-eln-load-path
        (list (expand-file-name "eln-cache" user-emacs-directory)))
  
  ;; 确保缓存目录存在
  (let ((eln-cache-dir (car native-comp-eln-load-path)))
    (unless (file-exists-p eln-cache-dir)
      (make-directory eln-cache-dir t)))
  
  ;; 设置原生编译的临时目录
  (setq native-comp-async-env-modifier-form
        `(setq temporary-file-directory
               ,(expand-file-name "tmp/" user-emacs-directory)))
  
  ;; 确保临时目录存在
  (let ((temp-dir (expand-file-name "tmp/" user-emacs-directory)))
    (unless (file-exists-p temp-dir)
      (make-directory temp-dir t)))
  
  ;; 原生编译配置
  (setq native-comp-deferred-compilation t
        native-comp-jit-compilation t
        native-comp-speed 2
        native-comp-async-report-warnings-errors nil
        native-comp-warning-on-missing-source nil
        native-comp-always-compile nil)

  ;; 修复 macOS 上 "error invoking gcc driver" 问题
  (when (eq system-type 'darwin)
    (let ((gcc-path (or (executable-find "gcc-14")
                        (executable-find "gcc-13")
                        (executable-find "gcc-12"))))
      (when gcc-path
        (setq native-comp-driver-options (list (concat "-B" (file-name-directory gcc-path))))
        (message "✓ Native Comp 编译器路径已修正: %s" gcc-path))))
  
  (message "✓ 原生编译已启用"))

(provide 'early-init)
;;; early-init.el ends here
