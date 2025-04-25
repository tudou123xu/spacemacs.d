;;; user-config.el --- 增强版开发环境配置 -*- lexical-binding: t; -*-

;; ==================== 系统核心检测 ====================
(defun number-of-processors ()
  "动态获取处理器核心数（适配 macOS/Linux/Windows）"
  (let ((default-threads 4))
    (cond
     ((and (spacemacs/system-is-mac) (executable-find "/usr/sbin/sysctl"))
      (string-to-number (shell-command-to-string "sysctl -n hw.logicalcpu")))
     ((executable-find "nproc")
      (string-to-number (shell-command-to-string "nproc")))
     ((eq system-type 'windows-nt)
      (string-to-number (getenv "NUMBER_OF_PROCESSORS")))
     (t default-threads))))  ; 默认值保障稳定性

;; ==================== 核心环境配置 ====================
(setq package-archives '(("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("gnu-cn"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
      package-check-signature 'allow-unsigned)

(unless (package-installed-p 'ob-go)
  (package-refresh-contents)
  (package-install 'ob-go))

;; ==================== Org-Babel 智能配置 ====================
(use-package org
  :defer 2
  :config
  (progn
    ;; 安全执行策略
    (setq org-confirm-babel-evaluate
          (lambda (lang _)
            (member lang '("python" "go" "shell"))))

    ;; 异步执行优化
    (setq org-babel-async-max-threads (max 4 (number-of-processors))
          org-babel-async-timeout 30)

    ;; 智能解释器选择
    (setq org-babel-python-command
          (cond ((executable-find "pdm") "pdm run python")
                ((executable-find "poetry") "poetry run python")
                (t "python3")))))

;; ==================== 语言支持模块 ====================
(use-package python
  :defer t
  :config
  (defun my/python-virtualenv-detect ()
    (when-let ((venv (or (getenv "VIRTUAL_ENV")
                         (locate-dominating-file default-directory ".python-version"))))
      (setq python-shell-virtualenv-path venv)))
  (add-hook 'python-mode-hook #'my/python-virtualenv-detect))

(use-package go-mode
  :if (executable-find "go")
  :config
  (setq gofmt-command "goimports"
        compile-command "go build -v && go test -v"))

;; ==================== 界面优化 ====================
;; 全局视觉增强（修复 visual-line-mode 失效问题）
(defun my/enable-smart-visuals ()
  (global-visual-line-mode 1)
  (setq truncate-lines nil)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

(add-hook 'after-init-hook #'my/enable-smart-visuals)  ; 确保在所有配置加载后启用

;; 跨平台字体渲染
(when (display-graphic-p)
  (cond ((spacemacs/system-is-mac)
         (set-face-attribute 'default nil :font "SF Mono-14")
         (set-fontset-font t 'han "PingFang SC"))
        ((spacemacs/system-is-mswindows)
         (dolist (charset '(han cjk-misc))
           (set-fontset-font t charset "Microsoft YaHei UI")))))

;; ==================== 系统集成优化 ====================
(use-package exec-path-from-shell
  :ensure t
  :config
  (progn
    (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PYTHONPATH"))
    (exec-path-from-shell-initialize)))

;; ==================== 安全审计模块 ====================
(defvar my/config-audit-log "~/.emacs.d/logs/config-audit.log")
(set-file-modes my/config-audit-log #o600)

(defun my/log-config-change ()
  (when (string= (buffer-file-name) (expand-file-name "user-config.el" user-emacs-directory))
    (append-to-file (format "[%s] %s@%s\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S")
                            (user-real-login-name)
                            (md5 (current-buffer)))
                    nil my/config-audit-log)))

(add-hook 'after-save-hook #'my/log-config-change)

;; ==================== 平台专属优化 ====================
(when (spacemacs/system-is-mac)
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(when (spacemacs/system-is-mswindows)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPersist=60 -o ConnectTimeout=15"))

;; ==================== 数据库模块加载 ====================
(add-to-list 'load-path "~/.spacemacs.d/private/db-layer")
(require 'config)

(provide 'user-config)
;;; user-config.el ends here
