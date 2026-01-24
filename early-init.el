;;; early-init.el --- Early Initialization for Spacemacs -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Description: Optimizations that must run before GUI startup

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

(provide 'early-init)
;;; early-init.el ends here
