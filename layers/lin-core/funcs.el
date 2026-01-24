;;; funcs.el --- lin-core layer functions file for Spacemacs. -*- lexical-binding: t; -*-

;; ==================== Error Handling Functions ====================

(defun lin-core/ensure-log-directory (log-file)
  "Ensure the directory for LOG-FILE exists and has correct permissions."
  (let ((log-dir (file-name-directory log-file)))
    (unless (file-exists-p log-dir)
      (make-directory log-dir t))
    (when (file-exists-p log-file)
      (set-file-modes log-file #o600))))

(defun lin-core/log-error (error-message &optional error-data)
  "Log ERROR-MESSAGE and optional ERROR-DATA to the error log file."
  (when (bound-and-true-p lin-core-enable-error-handling)
    (condition-case err
        (progn
          (lin-core/ensure-log-directory lin-core-error-log-file)
          (let ((log-entry (format "[%s] ERROR: %s%s\n"
                                   (format-time-string "%Y-%m-%d %H:%M:%S")
                                   error-message
                                   (if error-data (format " | Data: %s" error-data) ""))))
            (append-to-file log-entry nil lin-core-error-log-file)))
      (error (message "Failed to log error: %s" (error-message-string err))))))

(defun lin-core/safe-execute (func &optional fallback-func error-message)
  "Execute FUNC safely. If it fails, log ERROR-MESSAGE and try FALLBACK-FUNC."
  (condition-case err
      (funcall func)
    (error 
     (lin-core/log-error (or error-message (format "Function execution failed: %s" func))
                         (error-message-string err))
     (when fallback-func
       (condition-case fallback-err
           (funcall fallback-func)
         (error (lin-core/log-error "Fallback function also failed" (error-message-string fallback-err))))))))

;; ==================== Security Audit Functions ====================

(defun lin-core/log-config-change ()
  "Log changes to configuration files."
  (when (and buffer-file-name
             (string-match-p "\\.spacemacs\\.d" buffer-file-name)
             (string-suffix-p ".el" buffer-file-name))
    (condition-case err
        (progn
          (lin-core/ensure-log-directory lin-core-audit-log-file)
          (let ((log-entry (format "[%s] File: %s, User: %s, Hash: %s\n"
                                   (format-time-string "%Y-%m-%d %H:%M:%S")
                                   (file-name-nondirectory buffer-file-name)
                                   (user-real-login-name)
                                   (secure-hash 'md5 (current-buffer)))))
            (append-to-file log-entry nil lin-core-audit-log-file)))
      (error (message "Failed to log audit entry: %s" (error-message-string err))))))

(defun lin-core/security-check ()
  "Perform basic security checks."
  (when (and (file-exists-p lin-core-audit-log-file)
             (not (eq (file-modes lin-core-audit-log-file) #o600)))
    (set-file-modes lin-core-audit-log-file #o600)
    (message "Fixed permissions for audit log file")))

(defun lin-core/mask-sensitive-info (text)
  "Mask sensitive information in TEXT."
  (let ((patterns '(("password[[:space:]]*=[[:space:]]*\\([^[:space:]]+\\)" . "password=****")
                    ("token[[:space:]]*=[[:space:]]*\\([^[:space:]]+\\)" . "token=****")
                    ("api[_-]?key[[:space:]]*=[[:space:]]*\\([^[:space:]]+\\)" . "api_key=****"))))
    (dolist (pattern patterns)
      (setq text (replace-regexp-in-string (car pattern) (cdr pattern) text)))
    text))

;;; funcs.el ends here
