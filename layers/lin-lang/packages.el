;;; packages.el --- lin-lang layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: xuzhifeng
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst lin-lang-packages
  '(
    (org :location built-in)
    (python :location built-in)
    (go-mode :location built-in)
    (lsp-mode :location built-in)
    )
  "The list of Lisp packages required by the lin-lang layer.")

(defun lin-lang/post-init-org ()
  (with-eval-after-load 'org
    (progn
      ;; Safe execution policy
      (setq org-confirm-babel-evaluate
            (lambda (lang _)
              (not (member lang '("python" "go" "shell" "elisp")))))

      ;; Async execution optimization
      (setq org-babel-async-max-threads 4
            org-babel-async-timeout 30)

      ;; Smart interpreter selection
      (setq org-babel-python-command
            (cond ((executable-find "pdm") "pdm run python")
                  ((executable-find "poetry") "poetry run python")
                  ((executable-find "python3") "python3")
                  (t "python"))))))

(defun lin-lang/post-init-python ()
  (with-eval-after-load 'python
    (defun lin-lang/python-virtualenv-detect ()
      "Smartly detect Python virtual environment."
      (when-let ((venv (or (getenv "VIRTUAL_ENV")
                           (getenv "CONDA_DEFAULT_ENV")
                           (locate-dominating-file default-directory ".python-version")
                           (locate-dominating-file default-directory "pyproject.toml"))))
        (setq python-shell-virtualenv-path venv)
        (message "Detected Python environment: %s" venv)))
    (add-hook 'python-mode-hook #'lin-lang/python-virtualenv-detect)))

(defun lin-lang/post-init-go-mode ()
  (with-eval-after-load 'go-mode
    (when (executable-find "go")
      (setq gofmt-command (if (executable-find "goimports") "goimports" "gofmt")
            compile-command "go build -v && go test -v"
            go-test-args "-v"))))

(defun lin-lang/post-init-lsp-mode ()
  (with-eval-after-load 'lsp-mode
    (setq lsp-idle-delay 0.2                     ; Reduce response delay
          lsp-file-watch-threshold 2000          ; Increase file watch limit
          lsp-enable-file-watchers t
          lsp-keymap-prefix "C-c l"
          lsp-completion-provider :company)))

;;; packages.el ends here
