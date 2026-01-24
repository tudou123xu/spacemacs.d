;;; packages.el --- lin-ai layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: xuzhifeng
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst lin-ai-packages
  '(
    (aider :location local)
    (ellama :location local)
    )
  "The list of Lisp packages required by the lin-ai layer.")

(defun lin-ai/init-aider ()
  (use-package aider
    :defer t
    :config
    (lin-ai/configure-aider)))

(defun lin-ai/init-ellama ()
  (use-package ellama
    :defer t
    :init
    (setq ellama-provider 'ollama
          ellama-model "llama3:8b"
          ellama-api-base "http://localhost:11434")
    :config
    (spacemacs/set-leader-keys
      "oel" #'ellama-chat-new
      "oer" #'ellama-region-rewrite)))

;;; packages.el ends here
