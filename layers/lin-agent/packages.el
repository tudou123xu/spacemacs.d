;;; packages.el --- lin-agent layer packages file for Spacemacs.
;; Author: xuzhifeng
;;; License: GPLv3

(defconst lin-agent-packages
  '(
    plz
    transient
    (json :location built-in)
    )
  "The list of Lisp packages required by the lin-agent layer.
plz: HTTP client for Claude API.
transient: command palette (same engine as Magit).
json: built-in JSON encoding/decoding.")

(defun lin-agent/init-plz ()
  (use-package plz :defer t))

(defun lin-agent/post-init-transient ()
  (with-eval-after-load 'transient
    (require 'lin-agent-commands nil t)
    (when (fboundp 'lin-agent--define-transient)
      (lin-agent--define-transient))))

(defun lin-agent/post-init-json ()
  (require 'json))

;;; packages.el ends here
