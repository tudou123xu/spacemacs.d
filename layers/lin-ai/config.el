;;; config.el --- lin-ai layer keybindings for Spacemacs. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Cursor-style AI keybindings for Emacs.

;; ==================== Cursor-Style Global Keys (macOS) ====================
;; s- = Super/Command key on macOS
;;
;; s-k   → Inline Edit / abort gptel / close chat (Cursor: Cmd+K)
;; s-l   → AI Chat        (Cursor: Cmd+L)
;; s-i   → Agent Mode or close agent buffer (Cursor: Cmd+I)
;; s-;   → Send to AI     (Cursor: Cmd+Enter in chat)

(global-set-key (kbd "s-k") #'lin-ai/cursor-cmd-k)
(global-set-key (kbd "s-l") #'lin-ai/chat)
(global-set-key (kbd "s-i") (lambda () (interactive)
                              (cond
                               ((fboundp 'lin-agent/toggle-session)
                                (lin-agent/toggle-session))
                               ((fboundp 'lin-agent/start-session)
                                (lin-agent/start-session))
                               (t (lin-ai/chat)))))
(global-set-key (kbd "s-;") #'lin-ai/send-buffer-or-region)

;; ==================== Spacemacs Leader Keys ====================

(spacemacs/declare-prefix "oa" "AI Tools")

;; --- gptel: Core AI ---
(spacemacs/set-leader-keys
  ;; Chat & Send
  "oal" #'lin-ai/chat              ; AI chat panel
  "oas" #'lin-ai/send-buffer-or-region ; Send to AI
  "oam" #'lin-ai/switch-backend    ; gptel menu (switch model/backend)
  "oaq" #'lin-ai/quick-ask         ; Quick question

  ;; Inline Edit (Cursor Cmd+K style: abort / close chat / edit)
  "oak" #'lin-ai/cursor-cmd-k
  "oar" #'lin-ai/refactor          ; Refactor selection
  "oaf" #'lin-ai/fix-error         ; Fix error at point
  "oae" #'lin-ai/explain-code      ; Explain code
  "oad" #'lin-ai/generate-docstring ; Generate docstring
  "oaR" #'gptel-rewrite            ; Raw gptel rewrite

  ;; Abort
  "oax" #'gptel-abort              ; Abort current AI request

  ;; Aider (multi-file editing)
  "oai" #'aider-transient-menu     ; Aider menu
  "oaa" #'aider-add-current-file   ; Add file to aider

  ;; Router
  "oa." #'lin-ai/router            ; AI task router

  ;; API Key
  "oaK" #'lin-ai/set-key          ; Set API key
  ;; Cursor-style @attach (requires lin-agent layer for lin-agent-mentions)
  "oaA" (lambda () (interactive)
          (require 'lin-agent-mentions nil t)
          (if (fboundp 'lin-agent-insert-mention)
              (call-interactively #'lin-agent-insert-mention)
            (user-error "Enable lin-agent layer for @attach"))))

;; --- Aider auto-configuration ---
(with-eval-after-load 'aider
  (when (fboundp 'lin-ai/configure-aider)
    (lin-ai/configure-aider)))

;; --- gptel: Cursor-style @file / @folder in prompt (before context wrap) ---
(with-eval-after-load 'gptel
  (require 'cl-lib)
  (when (and (fboundp 'lin-ai--gptel-expand-mentions)
             (boundp 'gptel-prompt-transform-functions))
    (let ((defs (default-value 'gptel-prompt-transform-functions)))
      (unless (memq 'lin-ai--gptel-expand-mentions defs)
        (setq-default gptel-prompt-transform-functions
                      (let ((idx (cl-position 'gptel--transform-add-context defs)))
                        (if idx
                            (append (cl-subseq defs 0 idx)
                                    '(lin-ai--gptel-expand-mentions)
                                    (cl-subseq defs idx))
                          (append defs '(lin-ai--gptel-expand-mentions)))))))))

;;; config.el ends here
