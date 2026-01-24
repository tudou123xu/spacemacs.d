;;; config.el --- lin-core layer configuration file for Spacemacs. -*- lexical-binding: t; -*-

;; ==================== Error Handling Configuration ====================

(defvar lin-core-error-log-file 
  (expand-file-name "logs/error.log" user-emacs-directory)
  "Path to the error log file.")

(defvar lin-core-enable-error-handling t
  "Whether to enable error handling.")

(add-hook 'after-init-hook 
          (lambda ()
            (lin-core/ensure-log-directory lin-core-error-log-file)
            (message "Error handling module initialized")))

;; ==================== Security Audit Configuration ====================

(defvar lin-core-audit-log-file 
  (expand-file-name "logs/config-audit.log" user-emacs-directory)
  "Path to the configuration audit log file.")

(add-hook 'after-save-hook #'lin-core/log-config-change)
(add-hook 'after-init-hook #'lin-core/security-check)

;;; config.el ends here
