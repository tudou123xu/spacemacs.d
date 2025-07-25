;;; core-performance.el --- 核心性能优化模块 -*- lexical-binding: t; -*-

;; ==================== 系统核心检测 ====================
(defun my/number-of-processors ()
  "动态获取处理器核心数（跨平台兼容）"
  (let ((default-threads 4))
    (or (ignore-errors
          (cond
           ((and (spacemacs/system-is-mac) (executable-find "/usr/sbin/sysctl"))
            (string-to-number (shell-command-to-string "sysctl -n hw.logicalcpu")))
           ((executable-find "nproc")
            (string-to-number (shell-command-to-string "nproc")))
           ((eq system-type 'windows-nt)
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

;; 启动时优化GC，启动完成后恢复
(my/optimize-gc)
(add-hook 'emacs-startup-hook #'my/restore-gc)

;; ==================== 包管理优化 ====================
(setq package-archives '(("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("gnu-cn"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
      package-check-signature 'allow-unsigned
      package-enable-at-startup nil)

;; ==================== 文件处理优化 ====================
(setq read-process-output-max (* 3 1024 1024)  ; 3MB for LSP
      process-adaptive-read-buffering nil
      create-lockfiles nil                      ; 避免锁文件
      make-backup-files nil                     ; 禁用备份文件
      auto-save-default nil)                    ; 禁用自动保存

(provide 'core-performance)
;;; core-performance.el ends here 