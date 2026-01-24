;;; config.el --- lin-tools layer configuration file for Spacemacs. -*- lexical-binding: t; -*-

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
(setq large-file-warning-threshold (* 100 1024 1024)  ; 100MB
      vc-handled-backends '(Git)
      vc-follow-symlinks t)

;; ==================== Encoding ====================
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; ==================== macOS Specific ====================
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-function-modifier 'hyper)
  
  (setq insert-directory-program "/usr/local/bin/gls"
        dired-use-ls-dired t)
  
  (when (executable-find "mdfind")
    (setq locate-command "mdfind"))

  (global-set-key (kbd "s-r") #'lin-tools/reveal-in-finder)
  (global-set-key (kbd "s-w") #'delete-window)
  (global-set-key (kbd "s-q") #'save-buffers-kill-terminal)

  ;; Performance
  (setq mac-allow-anti-aliasing t
        frame-resize-pixelwise t)
  (setq ns-use-proxy-icon nil
        ns-use-mwheel-acceleration nil))

;;; config.el ends here
