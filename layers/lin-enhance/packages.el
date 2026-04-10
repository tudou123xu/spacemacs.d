;;; packages.el --- lin-enhance layer packages file for Spacemacs.
;; Author: xuzhifeng
;;; License: GPLv3

(defconst lin-enhance-packages
  '(
    (pyim :toggle (eq system-type 'darwin))
    )
  "The list of Lisp packages required by the lin-enhance layer.")

(defun lin-enhance/init-pyim ()
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
