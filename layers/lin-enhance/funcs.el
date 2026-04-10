;;; funcs.el --- lin-enhance layer functions file for Spacemacs. -*- lexical-binding: t; -*-
;; Author: xuzhifeng

(defun lin-enhance/reveal-in-finder ()
  "Reveal current file in Finder (macOS)."
  (interactive)
  (when (and (eq system-type 'darwin) buffer-file-name)
    (call-process "open" nil nil nil "-R" buffer-file-name)))

(defun lin-enhance/notify (title message)
  "Send system notification via osascript on macOS."
  (when (and (eq system-type 'darwin) (executable-find "osascript"))
    (call-process "osascript" nil nil nil
                  "-e" (format "display notification \"%s\" with title \"%s\""
                               message title))))

(defun lin-enhance/setup-cjk-fonts ()
  "Set CJK font fallback. English font is handled by dotspacemacs-default-font."
  (when (display-graphic-p)
    (let ((cjk-font (cond
                     ((eq system-type 'darwin) "PingFang SC")
                     ((eq system-type 'windows-nt) "Microsoft YaHei UI")
                     (t "Noto Sans CJK SC"))))
      (dolist (charset '(han cjk-misc bopomofo))
        (condition-case nil
            (set-fontset-font t charset cjk-font)
          (error nil))))))

;;; funcs.el ends here
