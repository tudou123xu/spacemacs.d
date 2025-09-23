;;; early-init.el --- 早期初始化配置 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 完全禁用原生编译 ====================
;; 在 Emacs 启动的最早期就禁用原生编译
(setq native-comp-deferred-compilation nil
      native-comp-jit-compilation nil
      native-comp-eln-load-path nil  ; 设置为 nil 而不是空列表
      comp-async-report-warnings-errors nil
      comp-deferred-compilation nil
      comp-deferred-compilation-black-list '(".*")
      comp-deferred-compilation-deny-list '(".*")
      native-comp-enable-subr-trampolines nil
      native-comp-speed 0)

;; 禁用原生编译相关的钩子
(when (boundp 'native-compile)
  (setq native-compile nil))

;; 设置环境变量
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; 禁用编译器警告
(setq comp-async-report-warnings-errors nil)

;; 清理原生编译缓存
(when (boundp 'native-comp-eln-load-path)
  (setq native-comp-eln-load-path nil))

;; 禁用所有编译相关功能
(when (fboundp 'native-compile)
  (defun native-compile (&rest args)
    (message "Native compilation disabled")
    nil))

;; 提供模块
(provide 'early-init)
;;; early-init.el ends here
