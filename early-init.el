;;; early-init.el --- 早期初始化配置 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024
;; Description: 在 Emacs 启动的最早期执行的配置

;; ==================== 环境变量设置 ====================
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; ==================== 原生编译优化 ====================
(setq native-comp-deferred-compilation t
      native-comp-jit-compilation t
      native-comp-speed 2
      comp-async-report-warnings-errors nil)

;; 设置原生编译缓存路径
(when (boundp 'native-comp-eln-load-path)
  (setq native-comp-eln-load-path
        (list (expand-file-name "eln-cache" user-emacs-directory))))

;; ==================== 模块声明 ====================
(provide 'early-init)
;;; early-init.el ends here
