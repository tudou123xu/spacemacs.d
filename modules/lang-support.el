;;; lang-support.el --- 编程语言支持模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== LSP 统一配置 ====================
(defun my/setup-lsp ()
  "配置 LSP 服务"
  (when (package-installed-p 'lsp-mode)
    (setq lsp-idle-delay 0.2
          lsp-file-watch-threshold 2000
          lsp-enable-file-watchers t
          lsp-keymap-prefix "C-c l"
          lsp-completion-provider :company)))

;; ==================== Python 配置 ====================
(defun my/setup-python ()
  "配置 Python 开发环境"
  (when (executable-find "python3")
    (setq python-shell-interpreter "python3"
          python-shell-prompt-detect-failure-warning nil)))

;; ==================== Go 配置 ====================
(defun my/setup-go ()
  "配置 Go 开发环境"
  (when (executable-find "go")
    (setq gofmt-command (if (executable-find "goimports") "goimports" "gofmt")
          compile-command "go build -v && go test -v")))

;; ==================== 初始化 ====================
(defun my/init-lang-support ()
  "初始化语言支持模块"
  (message "初始化语言支持模块...")
  
  ;; 配置各种语言
  (my/setup-lsp)
  (my/setup-python)
  (my/setup-go)
  
  (message "语言支持模块初始化完成"))

;; 延迟初始化
(run-with-timer 3 nil #'my/init-lang-support)

(provide 'lang-support)
;;; lang-support.el ends here