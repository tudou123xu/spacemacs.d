;;; funcs.el --- lin-ui layer functions file for Spacemacs. -*- lexical-binding: t; -*-

(defun lin-ui/enable-smart-visuals ()
  "Enable smart visual effects."
  (when (display-graphic-p)
    (global-visual-line-mode 1)
    (setq truncate-lines nil)
    (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
    
    ;; Smooth scrolling
    (setq scroll-conservatively 10000
          scroll-preserve-screen-position t
          scroll-margin 3)))

(defun lin-ui/setup-fonts ()
  "Cross-platform font configuration."
  (when (display-graphic-p)
    (let* ((font-size 14)
           (default-font 
             (cond ((eq system-type 'darwin) 
                    (if (member "SF Mono" (font-family-list))
                        (format "SF Mono-%d" font-size)
                      (format "Monaco-%d" font-size)))
                   ((eq system-type 'windows-nt)
                    (format "Consolas-%d" font-size))
                   (t (format "Source Code Pro-%d" font-size)))))
      
      ;; Set default font
      (condition-case nil
          (set-face-attribute 'default nil :font default-font)
        (error (message "Font setting failed, using default font")))
      
      ;; Chinese font configuration
      (when (eq system-type 'darwin)
        (dolist (charset '(han cjk-misc))
          (condition-case nil
              (set-fontset-font t charset "PingFang SC")
            (error nil))))
      
      (when (eq system-type 'windows-nt)
        (dolist (charset '(han cjk-misc))
          (condition-case nil
              (set-fontset-font t charset "Microsoft YaHei UI")
            (error nil)))))))

(defun lin-ui/theme-enhancements ()
  "Theme enhancements."
  ;; Highlight current line
  (global-hl-line-mode 1)
  
  ;; Line number optimization
  (when (version<= "26.1" emacs-version)
    (global-display-line-numbers-mode 1)
    (setq display-line-numbers-type 'relative))
  
  ;; Parenthesis matching
  (show-paren-mode 1)
  (setq show-paren-delay 0
        show-paren-style 'parenthesis))

;;; funcs.el ends here
