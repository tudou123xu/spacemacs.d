;;; config.el --- lin-ui layer configuration file for Spacemacs. -*- lexical-binding: t; -*-

(add-hook 'after-init-hook #'lin-ui/enable-smart-visuals)
(add-hook 'after-init-hook #'lin-ui/setup-fonts)
(add-hook 'after-init-hook #'lin-ui/theme-enhancements)

;;; config.el ends here
