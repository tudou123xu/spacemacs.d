;;; early-init.el --- 早期初始化配置 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024
;; Description: 在 Emacs 启动的最早期执行的配置

;; ==================== 环境变量设置 ====================
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; ==================== 原生编译优化 ====================
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  
  ;; 设置原生编译缓存路径（在 user-emacs-directory 下）
  (setq native-comp-eln-load-path
        (list (expand-file-name "eln-cache" user-emacs-directory)))
  
  ;; 确保缓存目录存在
  (let ((eln-cache-dir (car native-comp-eln-load-path)))
    (unless (file-exists-p eln-cache-dir)
      (make-directory eln-cache-dir t)))
  
  ;; 设置原生编译的临时目录（使用 user-emacs-directory 下的目录）
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
  
  (message "✓ 原生编译已启用，缓存目录: %s" (car native-comp-eln-load-path)))

;; 如果原生编译不可用，禁用相关功能
(unless (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
  (setq native-comp-deferred-compilation nil
        native-comp-jit-compilation nil)
  (message "⚠ 原生编译不可用，已禁用"))

;; ==================== 模块声明 ====================
(provide 'early-init)
;;; early-init.el ends here
