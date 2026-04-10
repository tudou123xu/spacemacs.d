;;; funcs.el --- lin-agent layer public interface. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Features: session lifecycle, slash commands, skills, cost tracking, modes.

(require 'cl-lib)

;; ==================== Session Management ====================

(defun lin-agent/start-session (&optional resume-p)
  "Start a new Claude Agent session. If RESUME-P is non-nil, preserve existing state."
  (interactive)
  (require 'agent-loop)
  (require 'tool-registry)
  (require 'context-manager)
  (require 'memory-bridge)
  (require 'lin-agent-commands)
  (require 'lin-agent-skills)
  (require 'lin-agent-mode)

  ;; Reset state only for new sessions
  (unless resume-p
    (setq lin-agent--messages nil
          lin-agent--turn-count 0
          lin-agent--mode 'agent)
    (lin-agent--reset-cost))

  ;; Initialize tools, skills, slash commands
  (lin-agent--init-builtin-tools)
  (when (and lin-agent-memory-enabled lin-agent-mcp--process)
    (lin-agent--init-memory-tools))
  (lin-agent--init-slash-commands)
  (lin-agent-skill-load-all)

  ;; Prepare buffer
  (let ((buf (get-buffer-create lin-agent--buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (lin-agent-mode)
      (insert "#+TITLE: Claude Agent Session\n")
      (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert (format "#+MODEL: %s\n" lin-agent-model))
      (insert (format "#+MODE: %s\n" lin-agent--mode))
      (insert (format "#+SKILLS: %s\n"
                      (if lin-agent--loaded-skills
                          (mapconcat (lambda (s) (plist-get s :name))
                                     lin-agent--loaded-skills ", ")
                        "none")))
      (insert (format "#+MEMORY: %s\n\n"
                      (if (and lin-agent-memory-enabled lin-agent-mcp--process)
                          "connected" "offline")))
      (insert "Type /help for slash commands.\n\n"))
    (display-buffer buf))

  (message "Claude Agent ready. Model: %s | Tools: %d | Skills: %d | Mode: %s | Memory: %s"
           lin-agent-model
           (hash-table-count lin-agent--tools)
           (length lin-agent--loaded-skills)
           lin-agent--mode
           (if (and lin-agent-memory-enabled lin-agent-mcp--process) "on" "off")))

(defun lin-agent/send-message (prompt)
  "Send PROMPT to the Claude Agent and trigger the agent loop.
Supports /slash commands as input."
  (interactive "sYou: ")
  (unless lin-agent--messages
    (lin-agent/start-session))

  ;; Slash command dispatch
  (unless (lin-agent--try-slash-command prompt)
    ;; Record in input history
    (when (fboundp 'lin-agent--input-ring-push)
      (lin-agent--input-ring-push prompt))
    ;; Auto-recall: inject relevant memories into the first message
    (when (and (null lin-agent--messages) lin-agent-auto-recall)
      (when-let ((recalled (lin-agent-memory--auto-recall prompt)))
        (lin-agent--display "[Auto-recalled relevant memories]\n")
        (setq prompt (concat recalled "\n\nUser request: " prompt))))

    (lin-agent--display "\n** You\n%s\n\n** Claude [%s]\n" prompt lin-agent--mode)

    ;; Build messages and run loop
    (let ((messages (append lin-agent--messages
                            (list (lin-agent--make-user-msg prompt))))
          (tools (if (memq lin-agent--mode '(plan ask))
                     (lin-agent--readonly-tool-definitions)
                   (lin-agent-tool-definitions))))
      (lin-agent--loop messages tools))))

(defun lin-agent--readonly-tool-definitions ()
  "Return only read-only tool definitions for plan/ask modes."
  (cl-remove-if-not
   (lambda (def)
     (lin-agent--tool-readonly-p
      (alist-get "name" def nil nil #'equal)))
   (lin-agent-tool-definitions)))

(defun lin-agent/stop ()
  "Stop the current Claude Agent session. Auto-saves session diary."
  (interactive)
  ;; Auto-diary: write session summary before clearing state
  (when lin-agent-auto-memory
    (lin-agent-memory--auto-diary))

  ;; Show final cost
  (when lin-agent--cost-data
    (lin-agent--display "\n─── Final Cost: $%.4f (in: %d, out: %d tokens) ───\n"
                        (plist-get lin-agent--cost-data :total-cost)
                        (plist-get lin-agent--cost-data :input-tokens)
                        (plist-get lin-agent--cost-data :output-tokens)))

  (setq lin-agent--messages nil
        lin-agent--turn-count 0)
  (message "Claude Agent session stopped."))

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
  "Switch the agent's model provider interactively."
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
