;;; lang-support.el --- 编程语言支持模块 -*- lexical-binding: t; -*-

;; ==================== Org-Babel 配置 ====================
(use-package org
  :defer 2
  :config
  (progn
    ;; 安全执行策略
    (setq org-confirm-babel-evaluate
          (lambda (lang _)
            (not (member lang '("python" "go" "shell" "elisp")))))

    ;; 异步执行优化
    (setq org-babel-async-max-threads (max 4 (my/number-of-processors))
          org-babel-async-timeout 30)

    ;; 智能解释器选择
    (setq org-babel-python-command
          (cond ((executable-find "pdm") "pdm run python")
                ((executable-find "poetry") "poetry run python")
                ((executable-find "python3") "python3")
                (t "python")))))

;; 确保 ob-go 可用
(unless (package-installed-p 'ob-go)
  (package-refresh-contents)
  (package-install 'ob-go))

;; ==================== Python 增强配置 ====================
(use-package python
  :defer t
  :config
  (defun my/python-virtualenv-detect ()
    "智能检测Python虚拟环境"
    (when-let ((venv (or (getenv "VIRTUAL_ENV")
                         (getenv "CONDA_DEFAULT_ENV")
                         (locate-dominating-file default-directory ".python-version")
                         (locate-dominating-file default-directory "pyproject.toml"))))
      (setq python-shell-virtualenv-path venv)
      (message "检测到Python环境: %s" venv)))
  
  (add-hook 'python-mode-hook #'my/python-virtualenv-detect))

;; ==================== Go 语言优化 ====================
(use-package go-mode
  :if (executable-find "go")
  :config
  (setq gofmt-command (if (executable-find "goimports") "goimports" "gofmt")
        compile-command "go build -v && go test -v"
        go-test-args "-v"))

;; ==================== LSP 统一配置 ====================
(with-eval-after-load 'lsp-mode
  (setq lsp-idle-delay 0.2                     ; 降低响应延迟
        lsp-file-watch-threshold 2000          ; 增加文件监控限制
        lsp-enable-file-watchers t
        lsp-keymap-prefix "C-c l"
        lsp-completion-provider :company))

(provide 'lang-support)
;;; lang-support.el ends here 