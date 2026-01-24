;;; funcs.el --- lin-tools layer functions file for Spacemacs. -*- lexical-binding: t; -*-

(defun lin-tools/notify (title message)
  "Send system notification."
  (cond
   ((eq system-type 'darwin)
    (when (executable-find "osascript")
      (call-process "osascript" nil nil nil
                    "-e" (format "display notification \"%s\" with title \"%s\"" 
                                message title))))))

(defun lin-tools/reveal-in-finder ()
  "Reveal current file in Finder (macOS)."
  (interactive)
  (when (and (eq system-type 'darwin) buffer-file-name)
    (call-process "open" nil nil nil "-R" buffer-file-name)))

;;; funcs.el ends here
