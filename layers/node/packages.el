;;; packages.el --- node layer for Spacemacs -*- lexical-binding: t; -*-
;;
;; Compatibility layer for dotfile declarations like:
;;   node
;;
;; NOTE:
;; In newer Spacemacs versions, `javascript` layer owns `nodejs-repl` and
;; `npm-mode`. Keeping this layer empty avoids "More than one init function"
;; warnings while still satisfying an existing `node` declaration.

(defconst node-packages
  '()
  "Compatibility layer: no packages declared.")

;;; packages.el ends here
