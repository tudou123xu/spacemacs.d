;;; funcs.el --- lin-agent layer public interface. -*- lexical-binding: t; -*-
;; Author: xuzhifeng

(require 'cl-lib)

;; ==================== Session State ====================

(defvar lin-agent--session-active nil
  "Non-nil when a session has been initialized.")

;; ==================== Session Management ====================

(defun lin-agent/start-session (&optional resume-p)
  "Start a new Claude Agent session. If RESUME-P, preserve state."
  (interactive)
  (require 'agent-loop)
  (require 'tool-registry)
  (require 'context-manager)
  (require 'memory-bridge)
  (require 'lin-agent-commands)
  (require 'lin-agent-skills)
  (require 'lin-agent-mode)

  (unless resume-p
    (setq lin-agent--messages nil
          lin-agent--turn-count 0
          lin-agent--mode 'agent)
    (lin-agent--reset-cost)
    (lin-agent--session-init))

  (lin-agent--init-builtin-tools)
  (when (and lin-agent-memory-enabled lin-agent-mcp--process)
    (lin-agent--init-memory-tools))
  (lin-agent--init-slash-commands)
  (lin-agent-skill-load-all)

  (let ((buf (get-buffer-create lin-agent--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (lin-agent-mode)
        (insert "#+TITLE: Claude Agent Session\n")
        (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
        (insert (format "#+PROVIDER: %s\n" lin-agent-provider))
        (insert (format "#+MODEL: %s\n" (lin-agent--current-model)))
        (insert (format "#+MODE: %s\n" lin-agent--mode))
        (insert (format "#+SKILLS: %s\n"
                        (if lin-agent--loaded-skills
                            (mapconcat (lambda (s) (plist-get s :name))
                                       lin-agent--loaded-skills ", ")
                          "none")))
        (insert (format "#+MEMORY: %s\n\n"
                        (if (and lin-agent-memory-enabled lin-agent-mcp--process)
                            "connected" "offline")))
        (insert "Cursor-style @file / @folder: type @path or M-x lin-agent-insert-mention.\n")
        (insert "Type /help for slash commands.\n\n")
        ;; Check for API key and show guidance if missing
        (unless (ignore-errors (lin-ai/get-key lin-agent-provider))
          (insert (format "⚠ API key not found for %s!\n" lin-agent-provider))
          (insert "  Set it now:        /key\n")
          (insert "  Switch provider:   /provider deepseek|siliconflow|claude\n")
          (insert "  Or create ~/.authinfo with:\n")
          (insert "    machine api.deepseek.com login deepseek password sk-YOUR-KEY\n")
          (insert "    machine api.siliconflow.cn login siliconflow password sk-YOUR-KEY\n")
          (insert "    machine api.anthropic.com login claude password sk-ant-YOUR-KEY\n\n"))
        ;; Insert the input prompt
        (lin-agent--insert-prompt)))
    (pop-to-buffer buf))

  (setq lin-agent--session-active t)

  (message "Claude Agent ready. Provider: %s | Model: %s | Tools: %d | Skills: %d | Mode: %s"
           lin-agent-provider
           (lin-agent--current-model)
           (hash-table-count lin-agent--tools)
           (length lin-agent--loaded-skills)
           lin-agent--mode))

(defun lin-agent/send-message (prompt)
  "Send PROMPT to the Claude Agent.
Supports /slash commands. If no session is active, starts one."
  (interactive "sYou: ")
  (when (or (null prompt) (string-empty-p (string-trim prompt)))
    (user-error "Empty message"))

  ;; Auto-start session if needed
  (unless lin-agent--session-active
    (lin-agent/start-session))

  ;; Slash command dispatch
  (if (lin-agent--try-slash-command prompt)
      ;; After slash command, re-insert the prompt
      (when (get-buffer lin-agent--buffer-name)
        (with-current-buffer lin-agent--buffer-name
          (lin-agent--insert-prompt)))
    (require 'lin-agent-mentions nil t)
    (when (fboundp 'lin-agent-expand-mentions)
      (setq prompt (lin-agent-expand-mentions prompt nil)))
    ;; Run user submit hooks (can modify prompt)
    (dolist (hook (or (bound-and-true-p lin-agent-user-submit-hooks) nil))
      (when-let ((modified (funcall hook prompt)))
        (setq prompt modified)))
    ;; Record in input history
    (when (fboundp 'lin-agent--input-ring-push)
      (lin-agent--input-ring-push prompt))
    ;; Auto-recall memories on first message
    (when (and (null lin-agent--messages) lin-agent-auto-recall)
      (when-let ((recalled (ignore-errors (lin-agent-memory--auto-recall prompt))))
        (lin-agent--display "[Auto-recalled relevant memories]\n")
        (setq prompt (concat recalled "\n\nUser request: " prompt))))

    (lin-agent--display "\n** You\n%s\n\n** %s [%s]\n" prompt
                        (capitalize (symbol-name lin-agent-provider)) lin-agent--mode)

    (let ((messages (append lin-agent--messages
                            (list (lin-agent--make-user-msg prompt))))
          (tools (if (memq lin-agent--mode '(plan ask))
                     (lin-agent--readonly-tool-definitions)
                   (lin-agent-tool-definitions))))
      (lin-agent--loop-dispatch messages tools))))

(defun lin-agent--readonly-tool-definitions ()
  "Return only read-only tool definitions for plan/ask modes."
  (cl-remove-if-not
   (lambda (def)
     (lin-agent--tool-readonly-p
      (alist-get "name" def nil nil #'equal)))
   (lin-agent-tool-definitions)))

(defun lin-agent/stop ()
  "Stop the current Claude Agent session."
  (interactive)
  ;; Stop streaming if active
  (when (fboundp 'lin-agent--stream-stop)
    (ignore-errors (lin-agent--stream-stop)))
  (when lin-agent-auto-memory
    (ignore-errors (lin-agent-memory--auto-diary)))

  (when lin-agent--cost-data
    (lin-agent--display "\n─── Final Cost: $%.4f (in: %d, out: %d tokens) ───\n"
                        (plist-get lin-agent--cost-data :total-cost)
                        (plist-get lin-agent--cost-data :input-tokens)
                        (plist-get lin-agent--cost-data :output-tokens)))

  ;; Run user stop hooks
  (dolist (hook (or (bound-and-true-p lin-agent-user-stop-hooks) nil))
    (ignore-errors (funcall hook)))
  (setq lin-agent--messages nil
        lin-agent--turn-count 0
        lin-agent--session-active nil)
  (message "Claude Agent session stopped."))

(defun lin-agent/close-session ()
  "Stop the agent, kill the session buffer, and delete its window if split.
Meant for the same global key that opens the session (e.g. Cmd+I)."
  (interactive)
  (unless (eq major-mode 'lin-agent-mode)
    (user-error "Not in a Claude Agent buffer; use Cmd+I from the agent buffer to close"))
  (let* ((buf (current-buffer))
         (win (get-buffer-window buf t)))
    (lin-agent/stop)
    (kill-buffer buf)
    ;; Reset default name so the next session starts from *Claude Agent*
    (when (boundp 'lin-agent--buffer-name)
      (setq lin-agent--buffer-name "*Claude Agent*"))
    (when (and win (window-live-p win)
               (> (length (window-list (window-frame win) 0)) 1))
      (delete-window win))))

(defun lin-agent/toggle-session ()
  "Start Claude Agent, or close it when already in `lin-agent-mode'.
Mirrors Cursor Cmd+I open/close in the agent buffer."
  (interactive)
  (if (eq major-mode 'lin-agent-mode)
      (lin-agent/close-session)
    (lin-agent/start-session)))

;; ==================== Utility Commands ====================

(defun lin-agent/list-tools ()
  "List all registered agent tools."
  (interactive)
  (with-current-buffer (get-buffer-create "*Agent Tools*")
    (erase-buffer)
    (insert "=== Registered Agent Tools ===\n\n")
    (maphash
     (lambda (name tool)
       (insert (format "  %s%s\n    %s\n\n"
                       name
                       (if (plist-get tool :read-only) " [read-only]" "")
                       (plist-get tool :description))))
     lin-agent--tools)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun lin-agent/switch-provider ()
  "Switch the agent's model provider."
  (interactive)
  (let* ((providers (lin-ai/available-providers))
         (choice (completing-read "Provider: "
                                  (mapcar #'symbol-name providers)
                                  nil t)))
    (when-let ((provider (lin-ai/get-provider (intern choice))))
      (setq lin-agent-model (plist-get provider :model))
      (setq lin-agent-api-endpoint
            (concat (plist-get provider :api-base)
                    (if (eq (intern choice) 'claude)
                        "/v1/messages"
                      "/chat/completions")))
      (message "Switched to %s (%s)" choice lin-agent-model))))

(defun lin-agent/toggle-auto-memory ()
  "Toggle automatic memory read/write."
  (interactive)
  (setq lin-agent-auto-memory (not lin-agent-auto-memory))
  (setq lin-agent-auto-recall lin-agent-auto-memory)
  (message "Auto-memory: %s" (if lin-agent-auto-memory "ON" "OFF")))

;;; funcs.el ends here
