;;; config.el --- lin-enhance layer configuration file for Spacemacs. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Spacemacs 不覆盖的增量定制：TRAMP、macOS 补充、中文字体

;; ==================== TRAMP Optimization ====================
(with-eval-after-load 'tramp
  (setq tramp-default-method "ssh"
        tramp-verbose 1
        tramp-use-connection-share t
        tramp-completion-reread-directory-timeout nil
        tramp-ssh-controlmaster-options
        (concat "-o ControlMaster=auto "
                "-o ControlPersist=60s "
                "-o ConnectTimeout=15 "
                "-o ServerAliveInterval=30")))

;; ==================== File Operations ====================
(setq large-file-warning-threshold (* 100 1024 1024)
      vc-handled-backends '(Git)
      vc-follow-symlinks t)

;; ==================== macOS Enhancements ====================
(when (eq system-type 'darwin)
  (setq insert-directory-program
        (or (executable-find "gls") "/usr/bin/ls"))
  (when (string-suffix-p "gls" (or insert-directory-program ""))
    (setq dired-use-ls-dired t))

  (when (executable-find "mdfind")
    (setq locate-command "mdfind"))

  (global-set-key (kbd "s-r") #'lin-enhance/reveal-in-finder)
  (global-set-key (kbd "s-w") #'delete-window)
  (global-set-key (kbd "s-q") #'save-buffers-kill-terminal)

  (setq mac-allow-anti-aliasing t
        frame-resize-pixelwise t
        ns-use-proxy-icon nil
        ns-use-mwheel-acceleration nil))

;; ==================== Chinese Font Fallback ====================
(add-hook 'after-init-hook #'lin-enhance/setup-cjk-fonts)

;;; config.el ends here
