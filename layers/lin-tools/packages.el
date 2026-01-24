;;; packages.el --- lin-tools layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: xuzhifeng
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst lin-tools-packages
  '(
    (exec-path-from-shell :toggle (or (eq system-type 'darwin) (eq system-type 'gnu/linux)))
    (pyim :toggle (eq system-type 'darwin))
    )
  "The list of Lisp packages required by the lin-tools layer.")

(defun lin-tools/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :config
    (progn
      (setq exec-path-from-shell-variables 
            '("PATH" "GOPATH" "GOROOT" "PYTHONPATH" "JAVA_HOME" "MAVEN_HOME"))
      (setq exec-path-from-shell-arguments '("-l"))
      (exec-path-from-shell-initialize))))

(defun lin-tools/init-pyim ()
  (use-package pyim
    :config
    (setq pyim-default-scheme 'quanpin
          pyim-page-tooltip 'posframe
          pyim-page-length 9)
    (setq pyim-fuzzy-pinyin-alist
          '(("en" "eng")
            ("in" "ing")
            ("l" "n")
            ("z" "zh")
            ("c" "ch")
            ("s" "sh")))
    (global-set-key (kbd "C-\\") 'toggle-input-method)))

;;; packages.el ends here
