;;; system-integration.el --- 系统集成模块 -*- lexical-binding: t; -*-

;; ==================== 环境变量处理 ====================
(use-package exec-path-from-shell
  :ensure t
  :if (or (my/system-is-mac) (my/system-is-linux))
  :config
  (progn
    (setq exec-path-from-shell-variables 
          '("PATH" "GOPATH" "GOROOT" "PYTHONPATH" "JAVA_HOME" "MAVEN_HOME"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; ==================== TRAMP 优化 ====================
(with-eval-after-load 'tramp
  (setq tramp-default-method "ssh"
        tramp-verbose 1                        ; 降低日志级别
        tramp-use-connection-share t
        tramp-completion-reread-directory-timeout nil
        tramp-ssh-controlmaster-options 
        (concat "-o ControlMaster=auto "
                "-o ControlPersist=60s "
                "-o ConnectTimeout=15 "
                "-o ServerAliveInterval=30")))

;; ==================== 文件操作优化 ====================
(setq large-file-warning-threshold (* 100 1024 1024)  ; 100MB
      vc-handled-backends '(Git)                       ; 只使用Git
      vc-follow-symlinks t)                           ; 自动跟随符号链接

;; ==================== 编码设置 ====================
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; ==================== 网络优化 ====================
(setq url-proxy-services nil)  ; 根据需要启用代理
(when (and (boundp 'socks-server) socks-server)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost" "127.0.0.1" "::1")))

(provide 'system-integration)
;;; system-integration.el ends here 