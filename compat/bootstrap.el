;;; bootstrap.el --- Compatibility bootstrap -*- lexical-binding: t; -*-
;;
;; Goal: keep `init.el` stable & minimal by centralizing compatibility shims.
;; This file is loaded early from `dotspacemacs/user-init`.

;; ------------------------
;; Spacemacs internal state
;; ------------------------
;; Some Spacemacs versions expect these internal vars to exist during layer
;; pre-config steps. When they don't, it crashes with (void-variable ...).
(defvar spacemacs--dap-supported-modes nil
  "List of major modes supported by dap-mode (internal Spacemacs state).")

(defvar spacemacs--web-beautify-modes nil
  "List of major modes supported by web-beautify (internal Spacemacs state).")

;; ------------------------
;; Shell layer compatibility
;; ------------------------
;; Keep these defined even if upstream changes.
(defvar shell-enable-vterm-support t
  "Enable vterm support for shell layer.")

(defvar shell-default-full-span nil
  "Whether shell-pop window spans full frame.")

(defvar shell-protect-eshell-prompt nil
  "Whether to protect eshell prompt (Spacemacs shell layer).")

;; Older/variant shell-pop recipes rely on this macro.
(unless (fboundp 'make-shell-pop-command)
  (defmacro make-shell-pop-command (func &optional shell shell-term-shell)
    (let* ((name (if (stringp func) func (symbol-name func)))
           (func-name (intern (format "spacemacs/shell-pop-%s" name))))
      `(defun ,func-name (index)
         (interactive "P")
         (require 'shell-pop)
         (shell-pop--set-shell-type ,name ,shell ,shell-term-shell)
         (shell-pop index)))))

;; Some layers assume this helper exists.
(unless (fboundp 'spacemacs/disable-hl-line-mode)
  (defun spacemacs/disable-hl-line-mode ()
    (when (fboundp 'hl-line-mode)
      (hl-line-mode -1))))

;; ------------------------
;; Org layer compatibility
;; ------------------------
;; Spacemacs org layer expects `org-clocks-prefix` to return a string.
(unless (fboundp 'org-clocks-prefix)
  (defun org-clocks-prefix (&rest _args)
    "Prefix description for org clock commands."
    "clock"))

(provide 'lin-compat-bootstrap)
;;; bootstrap.el ends here
