;;; lin-agent-commands.el --- Agent slash commands & transient palette. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Aligns with Claude Code's commands.ts: /slash commands + transient palette.

(require 'transient)
(require 'cl-lib)
(require 'subr-x)

;; ==================== Slash Command Registry ====================

(defvar lin-agent--slash-commands nil
  "Registry of slash commands: list of (name . plist) where plist has :handler :description.")

(defun lin-agent-slash-register (name description handler)
  "Register a slash command NAME with DESCRIPTION and HANDLER function."
  (setq lin-agent--slash-commands
        (cons (cons name (list :description description :handler handler))
              (cl-remove name lin-agent--slash-commands :key #'car :test #'equal))))

(defun lin-agent--init-slash-commands ()
  "Register built-in slash commands."
  (lin-agent-slash-register "/help" "Show available slash commands"
    (lambda (_args)
      (lin-agent--display "\n=== Slash Commands ===\n")
      (dolist (cmd (reverse lin-agent--slash-commands))
        (lin-agent--display "  %-16s %s\n"
                            (car cmd)
                            (plist-get (cdr cmd) :description)))
      (lin-agent--display "\n")))

  (lin-agent-slash-register "/compact" "Compact conversation context"
    (lambda (_args)
      (lin-agent/compact-context)))

  (lin-agent-slash-register "/cost" "Show session cost"
    (lambda (_args)
      (lin-agent/show-cost)))

  (lin-agent-slash-register "/mode" "Switch mode (agent/plan/ask)"
    (lambda (args)
      (let ((mode (or (car args) "agent")))
        (lin-agent/set-mode (intern mode)))))

  (lin-agent-slash-register "/tools" "List available tools"
    (lambda (_args)
      (lin-agent/list-tools)))

  (lin-agent-slash-register "/skills" "List or load skills"
    (lambda (args)
      (if (car args)
          (lin-agent-skill-load-by-name (car args))
        (lin-agent/list-skills))))

  (lin-agent-slash-register "/save" "Save current session"
    (lambda (_args)
      (lin-agent/save-session)))

  (lin-agent-slash-register "/resume" "Resume a saved session"
    (lambda (_args)
      (lin-agent/resume-session)))

  (lin-agent-slash-register "/clear" "Clear conversation history"
    (lambda (_args)
      (setq lin-agent--messages nil)
      (setq lin-agent--turn-count 0)
      (lin-agent--reset-cost)
      (lin-agent--display "\n[Session cleared]\n")))

  (lin-agent-slash-register "/memory" "Search or store memory"
    (lambda (args)
      (if (car args)
          (lin-agent/memory-search (string-join args " "))
        (call-interactively #'lin-agent/memory-search))))

  (lin-agent-slash-register "/copy" "Copy last Claude response to clipboard"
    (lambda (_args)
      (if (fboundp 'lin-agent/copy-last-response)
          (lin-agent/copy-last-response)
        (message "lin-agent-mode not loaded."))))

  (lin-agent-slash-register "/rename" "Rename the current session"
    (lambda (args)
      (let ((name (if args (string-join args " ")
                    (read-string "Session name: "))))
        (if (fboundp 'lin-agent/rename-session)
            (lin-agent/rename-session name)
          (message "lin-agent-mode not loaded.")))))

  (lin-agent-slash-register "/context" "Dump full context to inspector buffer"
    (lambda (_args)
      (lin-agent/inspect-context)))

  (lin-agent-slash-register "/model" "Switch model: /model deepseek-chat or /model (interactive)"
    (lambda (args)
      (if (car args)
          (progn (setq lin-agent-model (car args))
                 (lin-agent--display "\n[Model → %s (provider: %s)]\n" lin-agent-model lin-agent-provider))
        (let ((model (read-string (format "Model name (current: %s): " (lin-agent--current-model)))))
          (when (not (string-empty-p model))
            (setq lin-agent-model model)
            (lin-agent--display "\n[Model → %s]\n" model))))))

  (lin-agent-slash-register "/rewind" "Rewind last N turns (default 1)"
    (lambda (args)
      (let ((n (if (car args) (string-to-number (car args)) 1)))
        (lin-agent/rewind n))))

  (lin-agent-slash-register "/status" "Show session status summary"
    (lambda (_args)
      (lin-agent--display "\n=== Session Status ===\n")
      (lin-agent--display "  Provider: %s\n" lin-agent-provider)
      (lin-agent--display "  Model: %s\n" (lin-agent--current-model))
      (lin-agent--display "  API: %s\n" (lin-agent--current-api-type))
      (lin-agent--display "  Mode:  %s\n" lin-agent--mode)
      (lin-agent--display "  Turns: %d\n" lin-agent--turn-count)
      (lin-agent--display "  Messages: %d\n" (length lin-agent--messages))
      (lin-agent--display "  %s\n" (lin-agent--token-budget-status))
      (when lin-agent--cost-data
        (lin-agent--display "  Cost: $%.4f\n" (plist-get lin-agent--cost-data :total-cost)))
      (lin-agent--display "\n")))

  (lin-agent-slash-register "/stats" "Show detailed token and cost statistics"
    (lambda (_args)
      (lin-agent--display "\n=== Session Statistics ===\n")
      (when lin-agent--cost-data
        (lin-agent--display "  Input tokens:  %d\n" (plist-get lin-agent--cost-data :input-tokens))
        (lin-agent--display "  Output tokens: %d\n" (plist-get lin-agent--cost-data :output-tokens))
        (lin-agent--display "  Total cost:    $%.4f\n" (plist-get lin-agent--cost-data :total-cost)))
      (lin-agent--display "  %s\n" (lin-agent--token-budget-status))
      (lin-agent--display "  Turns: %d | Messages: %d\n\n" lin-agent--turn-count (length lin-agent--messages))))

  (lin-agent-slash-register "/diff" "Show git diff. Args: [staged|HEAD~N|branch] [path]"
    (lambda (args)
      (let* ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                       default-directory))
             (target (or (car args) ""))
             (path-arg (cadr args))
             (cmd (format "git -C %s diff %s%s 2>&1 | head -200"
                          (shell-quote-argument root)
                          (if (equal target "staged") "--cached " target)
                          (if path-arg (format " -- %s" (shell-quote-argument path-arg)) "")))
             (diff (shell-command-to-string cmd)))
        (lin-agent--display "\n=== Git Diff%s ===\n%s\n"
                            (if (not (string-empty-p target)) (format " (%s)" target) "")
                            diff))))

  (lin-agent-slash-register "/btw" "Quick side question (no tools, one turn)"
    (lambda (args)
      (let ((question (if args (string-join args " ")
                        (read-string "Side question: "))))
        (when (and question (not (string-empty-p question)))
          (lin-agent--display "\n** Side Question\n%s\n\n" question)
          (require 'agent-loop)
          (let* ((msgs (list (lin-agent--make-user-msg
                              (format "Quick side question (answer briefly, no tools needed): %s" question))))
                 (response (lin-agent--call-api msgs nil)))
            (let ((text (lin-agent--extract-text response)))
              (when text
                (lin-agent--display "** Answer\n%s\n" text))))))))

  (lin-agent-slash-register "/files" "List project files (top-level)"
    (lambda (_args)
      (let* ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                        default-directory))
             (files (directory-files root nil "^[^.]")))
        (lin-agent--display "\n=== Project Files (%s) ===\n" root)
        (dolist (f files)
          (lin-agent--display "  %s%s\n" f (if (file-directory-p (expand-file-name f root)) "/" "")))
        (lin-agent--display "\n"))))

  (lin-agent-slash-register "/exit" "End session and close the agent buffer (Claude Code /exit)"
    (lambda (_args)
      (if (eq major-mode 'lin-agent-mode)
          (if (fboundp 'lin-agent/close-session)
              (lin-agent/close-session)
            (lin-agent/stop))
        (when (fboundp 'lin-agent/stop)
          (lin-agent/stop)))))

  (lin-agent-slash-register "/plan" "Switch to plan mode (read-only analysis)"
    (lambda (_args)
      (lin-agent/set-mode 'plan)
      (lin-agent--display "\n[Mode → plan]\n")))

  (lin-agent-slash-register "/session" "Show session id, buffer, and transcript file"
    (lambda (_args)
      (lin-agent--display "\n=== Session ===\n")
      (lin-agent--display "  lin-agent:  %s\n" (if (boundp 'lin-agent-version) lin-agent-version "unknown"))
      (lin-agent--display "  Buffer:     %s\n" (if (boundp 'lin-agent--buffer-name) lin-agent--buffer-name "—"))
      (when (boundp 'lin-agent--session-id)
        (lin-agent--display "  Session id: %s\n" lin-agent--session-id))
      (when (boundp 'lin-agent--session-file)
        (lin-agent--display "  Transcript: %s\n" (or lin-agent--session-file "—")))
      (lin-agent--display "  Active:     %s\n" (if (and (boundp 'lin-agent--session-active) lin-agent--session-active) "yes" "no"))
      (lin-agent--display "\n")))

  (lin-agent-slash-register "/version" "Show Emacs and lin-agent version"
    (lambda (_args)
      (lin-agent--display "\n=== Version ===\n")
      (lin-agent--display "  lin-agent: %s\n" (if (boundp 'lin-agent-version) lin-agent-version "unknown"))
      (lin-agent--display "  Emacs:     %s\n" emacs-version)
      (lin-agent--display "\n")))

  (lin-agent-slash-register "/usage" "Token and cost usage (alias for status + stats)"
    (lambda (_args)
      (lin-agent--display "\n=== Usage ===\n")
      (lin-agent--display "  Provider: %s | Model: %s\n" lin-agent-provider (lin-agent--current-model))
      (lin-agent--display "  Turns: %d | Messages: %d\n" lin-agent--turn-count (length lin-agent--messages))
      (lin-agent--display "  %s\n" (lin-agent--token-budget-status))
      (when lin-agent--cost-data
        (lin-agent--display "  Input tokens:  %d\n" (plist-get lin-agent--cost-data :input-tokens))
        (lin-agent--display "  Output tokens: %d\n" (plist-get lin-agent--cost-data :output-tokens))
        (lin-agent--display "  Total cost:    $%.4f\n" (plist-get lin-agent--cost-data :total-cost)))
      (lin-agent--display "\n")))

  (lin-agent-slash-register "/worktree" "List git worktrees (git worktree list)"
    (lambda (_args)
      (let* ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                       default-directory))
             (out (shell-command-to-string
                   (format "git -C %s worktree list 2>&1" (shell-quote-argument root)))))
        (lin-agent--display "\n=== Git worktrees ===\n%s\n" out))))

  (lin-agent-slash-register "/doctor" "Run diagnostics on the agent configuration"
    (lambda (_args)
      (lin-agent--display "\n=== Agent Doctor ===\n")
      (lin-agent--display "  lin-agent:  %s\n" (if (boundp 'lin-agent-version) lin-agent-version "unknown"))
      (lin-agent--display "  Provider:   %s (%s)\n" lin-agent-provider (lin-agent--current-api-type))
      (lin-agent--display "  API key:    %s\n"
                          (if (ignore-errors (lin-ai/get-key lin-agent-provider)) "OK" "MISSING"))
      (lin-agent--display "  Model:      %s\n" (lin-agent--current-model))
      (lin-agent--display "  Endpoint:   %s\n" (lin-agent--current-endpoint))
      (lin-agent--display "  Tools:      %d registered\n" (hash-table-count lin-agent--tools))
      (lin-agent--display "  MCP Memory: %s\n"
                          (if (and (boundp 'lin-agent-mcp--process) lin-agent-mcp--process)
                              "connected" "offline"))
      (lin-agent--display "  Skills:     %d loaded\n"
                          (if (boundp 'lin-agent--loaded-skills)
                              (length lin-agent--loaded-skills) 0))
      (lin-agent--display "  %s\n" (lin-agent--token-budget-status))
      (lin-agent--display "  Emacs:      %s\n" emacs-version)
      (lin-agent--display "  plz.el:     %s\n" (if (featurep 'plz) "loaded" "not loaded"))
      (lin-agent--display "  Threads:    %s\n" (if (fboundp 'make-thread) "available" "unavailable"))
      (lin-agent--display "\n")))

  (lin-agent-slash-register "/export" "Export conversation to an org file"
    (lambda (_args)
      (let ((path (expand-file-name
                   (format "agent-export-%s.org" (format-time-string "%Y%m%d-%H%M%S"))
                   (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                       "~/"))))
        (with-temp-file path
          (insert "#+TITLE: Agent Export\n")
          (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
          (dolist (msg lin-agent--messages)
            (let ((role (alist-get 'role msg))
                  (content (alist-get 'content msg)))
              (insert (format "** %s\n" (upcase (or role "?"))))
              (cond
               ((stringp content) (insert content))
               ((vectorp content)
                (mapc (lambda (block)
                        (let ((type (alist-get 'type block)))
                          (cond
                           ((equal type "text")
                            (insert (or (alist-get 'text block) "")))
                           ((equal type "tool_use")
                            (insert (format "#+begin_src\n[tool: %s]\n%s\n#+end_src\n"
                                            (alist-get 'name block)
                                            (json-encode (alist-get 'input block)))))
                           ((equal type "tool_result")
                            (let ((c (alist-get 'content block)))
                              (insert (format "#+begin_example\n%s\n#+end_example\n"
                                              (if (stringp c) (substring c 0 (min 500 (length c))) ""))))))))
                      content)))
              (insert "\n\n"))))
        (lin-agent--display "\n[Exported to %s]\n" path))))

  (lin-agent-slash-register "/effort" "Set output style: concise, normal, detailed, educational"
    (lambda (args)
      (let* ((style (if (car args)
                        (intern (car args))
                      (intern (completing-read "Style: " '("concise" "normal" "detailed" "educational") nil t))))
             (desc (alist-get style lin-agent-output-styles)))
        (if desc
            (progn
              (setq lin-agent-output-style style)
              (lin-agent--display "\n[Output style → %s]\n" style))
          (lin-agent--display "\n[Unknown style: %s. Options: concise, normal, detailed, educational]\n" style)))))

  (lin-agent-slash-register "/init" "Create a .lin-agent.md project config file"
    (lambda (_args)
      (let* ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                       default-directory))
             (path (expand-file-name ".lin-agent.md" root)))
        (if (file-exists-p path)
            (progn
              (find-file path)
              (lin-agent--display "\n[Opened existing %s]\n" path))
          (with-temp-file path
            (insert "# Project Instructions for Claude Agent\n\n"
                    "## Context\n"
                    "<!-- Describe your project, tech stack, conventions -->\n\n"
                    "## Preferences\n"
                    "<!-- Coding style, naming, testing preferences -->\n\n"
                    "## Rules\n"
                    "<!-- Specific rules for this project -->\n\n"))
          (find-file path)
          (lin-agent--display "\n[Created %s]\n" path)))))

  (lin-agent-slash-register "/branch" "Show or switch git branch"
    (lambda (args)
      (let* ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                       default-directory))
             (current (string-trim
                       (shell-command-to-string
                        (format "git -C %s rev-parse --abbrev-ref HEAD 2>/dev/null"
                                (shell-quote-argument root))))))
        (if (car args)
            (let ((output (shell-command-to-string
                           (format "git -C %s checkout %s 2>&1"
                                   (shell-quote-argument root)
                                   (shell-quote-argument (car args))))))
              (lin-agent--display "\n%s\n" output))
          (let ((branches (string-trim
                           (shell-command-to-string
                            (format "git -C %s branch --format='%%(refname:short)' 2>/dev/null"
                                    (shell-quote-argument root))))))
            (lin-agent--display "\n=== Git Branches ===\n")
            (lin-agent--display "Current: %s\n" current)
            (lin-agent--display "%s\n" branches))))))

  (lin-agent-slash-register "/permissions" "Set permission mode: default, accept-edits, bypass, plan"
    (lambda (args)
      (let ((mode (if (car args)
                      (intern (car args))
                    (intern (completing-read "Permission mode: "
                                            '("default" "accept-edits" "bypass" "plan") nil t)))))
        (if (memq mode '(default accept-edits bypass plan))
            (progn
              (setq lin-agent-permission-mode mode)
              (lin-agent--display "\n[Permission mode → %s]\n" mode))
          (lin-agent--display "\n[Unknown mode: %s]\n" mode)))))

  (lin-agent-slash-register "/mcp" "MCP: connect|disconnect|resources [SERVER]"
    (lambda (args)
      (require 'mcp-client)
      (pcase (car args)
        ("connect"
         (if (cadr args)
             (lin-agent-mcp-connect (cadr args))
           (call-interactively #'lin-agent-mcp-connect)))
        ("disconnect"
         (if (cadr args)
             (lin-agent-mcp-disconnect (cadr args))
           (call-interactively #'lin-agent-mcp-disconnect)))
        ("resources"
         (if (cadr args)
             (condition-case err
                 (let ((res (lin-agent-mcp-list-resources (cadr args))))
                   (lin-agent--display "\n=== MCP resources (%s) ===\n%s\n" (cadr args)
                                       (if res
                                           (mapconcat (lambda (r)
                                                        (format "%s  %s"
                                                                (or (alist-get 'uri r) "?")
                                                                (or (alist-get 'name r) "")))
                                                      (if (vectorp res) (append res nil) res) "\n")
                                         "(none)")))
               (error (lin-agent--display "\n[mcp resources error: %s]\n" (error-message-string err))))
           (lin-agent--display "\n%s\n" (lin-agent-mcp-list-resources-all-formatted))))
        (_
         (let ((active (lin-agent-mcp-active-servers))
               (configs (mapcar #'car lin-agent-mcp-server-configs)))
           (lin-agent--display "\n=== MCP Servers ===\n")
           (lin-agent--display "Configured: %s\n" (if configs (string-join configs ", ") "(none)"))
           (lin-agent--display "Active: %s\n" (if active (string-join active ", ") "(none)"))
           (lin-agent--display "\nUsage: /mcp connect NAME | /mcp disconnect NAME | /mcp resources [NAME]\n"))))))

  (lin-agent-slash-register "/feedback" "Send feedback about the agent experience"
    (lambda (args)
      (let ((feedback (if args (string-join args " ")
                        (read-string "Feedback: "))))
        (when (and feedback (not (string-empty-p feedback)))
          (when (fboundp 'lin-agent-memory--store)
            (condition-case nil
                (lin-agent-mcp--call-sync "mempalace_add_drawer"
                  `(("wing" . "agent-feedback")
                    ("room" . "user-feedback")
                    ("content" . ,feedback)))
              (error nil)))
          (lin-agent--display "\n[Feedback recorded. Thank you!]\n")))))

  (lin-agent-slash-register "/keybindings" "Show available keybindings"
    (lambda (_args)
      (lin-agent--display "\n=== Agent Keybindings ===\n")
      (lin-agent--display "SPC o c a  Start session\n")
      (lin-agent--display "SPC o c s  Send message\n")
      (lin-agent--display "SPC o c q  Stop session\n")
      (lin-agent--display "SPC o c .  Command palette (transient)\n")
      (lin-agent--display "SPC o c 1  Agent mode\n")
      (lin-agent--display "SPC o c 2  Plan mode\n")
      (lin-agent--display "SPC o c 3  Ask mode\n")
      (lin-agent--display "SPC o c m  Memory search\n")
      (lin-agent--display "SPC o c w  Memory store\n")
      (lin-agent--display "SPC o c C  Memory connect\n")
      (lin-agent--display "SPC o c D  Memory disconnect\n")
      (lin-agent--display "SPC o c t  List tools\n")
      (lin-agent--display "SPC o c c  Compact context\n")
      (lin-agent--display "SPC o c i  Inspect context\n")
      (lin-agent--display "SPC o c r  Rewind turns\n")
      (lin-agent--display "SPC o c k  List skills\n")
      (lin-agent--display "SPC o c p  Switch provider\n")
      (lin-agent--display "SPC o c S  Save session\n")
      (lin-agent--display "SPC o c R  Resume session\n")
      (lin-agent--display "SPC o c $  Show cost\n")
      (lin-agent--display "SPC o c B  Token budget\n")
      (lin-agent--display "SPC o c n  Rename session\n")
      (lin-agent--display "SPC o c y  Copy last response\n")
      (lin-agent--display "\n--- In Agent Buffer ---\n")
      (lin-agent--display "RET / C-c C-c  Send message\n")
      (lin-agent--display "M-p / M-n      Input history\n")
      (lin-agent--display "C-c C-k        Stop\n")
      (lin-agent--display "C-c C-w        Copy response\n")
      (lin-agent--display "C-c C-s        Save session\n")
      (lin-agent--display "C-c C-r        Rewind\n")
      (lin-agent--display "C-c C-i        Inspect context\n")
      (lin-agent--display "C-c C-e        Export\n")
      (lin-agent--display "C-c C-d        Doctor\n")
      (lin-agent--display "q              Quit window\n")
      (lin-agent--display "@path / C-c C-a / SPC o c A  Attach file or folder (Cursor-style)\n")
      (lin-agent--display "\n--- macOS (lin-ai layer) ---\n")
      (lin-agent--display "Cmd+I  Toggle agent session (close when in agent buffer)\n")
      (lin-agent--display "Cmd+K  Inline edit / abort gptel / close AI chat\n")
      (lin-agent--display "\n--- Claude Code-style slash ---\n")
      (lin-agent--display "/exit /plan /session /version /usage /worktree\n")
      (lin-agent--display "Tools: mcp_list_resources, mcp_read_resource, notebook_edit (.ipynb)\n")))

  (lin-agent-slash-register "/security-review" "Ask LLM to do a security review of recent changes"
    (lambda (_args)
      (let* ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                       default-directory))
             (diff (shell-command-to-string
                    (format "git -C %s diff HEAD~3 --stat 2>/dev/null | head -50" (shell-quote-argument root)))))
        (lin-agent/send-message
         (format "Please perform a security review of the following recent changes. \
Look for: hardcoded secrets, SQL injection, XSS, path traversal, insecure deserialization, \
missing auth checks, and other OWASP Top 10 risks.\n\nChanged files:\n%s\n\nRead each changed file and report findings." diff)))))

  (lin-agent-slash-register "/add-dir" "Add a directory to the project context"
    (lambda (args)
      (let ((dir (if (car args) (expand-file-name (car args))
                   (read-directory-name "Add directory: "))))
        (if (file-directory-p dir)
            (let ((files (directory-files dir nil "^[^.]")))
              (lin-agent--display "\n[Added %s to context: %d entries]\n" dir (length files))
              (lin-agent--display "%s\n" (mapconcat (lambda (f) (format "  %s" f))
                                                     (cl-subseq files 0 (min 20 (length files))) "\n")))
          (lin-agent--display "\n[Directory not found: %s]\n" dir)))))

  (lin-agent-slash-register "/tasks" "List all tasks (or filter: /tasks pending|completed)"
    (lambda (args)
      (let* ((handler (lin-agent-tool-get-handler "task_list"))
             (status (or (car args) "all"))
             (result (funcall handler `((status . ,status)))))
        (lin-agent--display "\n=== Tasks (%s) ===\n%s\n\n" status result))))

  (lin-agent-slash-register "/key" "Set API key for current provider (or specify: /key deepseek)"
    (lambda (args)
      (let* ((provider (if (car args) (intern (car args)) lin-agent-provider))
             (key (read-passwd (format "%s API key: " provider))))
        (when (and key (not (string-empty-p key)))
          (lin-ai/set-key provider key)
          (lin-agent--display "\n[%s API key set for this session]\n" provider)))))

  (lin-agent-slash-register "/provider" "Switch AI provider: /provider deepseek|siliconflow|claude|openai"
    (lambda (args)
      (let* ((available (mapcar (lambda (c) (symbol-name (car c))) lin-agent-provider-configs))
             (choice (if (car args) (car args)
                       (completing-read "Provider: " available nil t))))
        (setq lin-agent-provider (intern choice))
        (setq lin-agent-model nil
              lin-agent-api-endpoint nil)
        (lin-agent--display "\n[Provider → %s | Model: %s | Endpoint: %s]\n"
                            choice
                            (lin-agent--current-model)
                            (lin-agent--current-endpoint))
        (unless (lin-ai/get-key lin-agent-provider)
          (lin-agent--display "[⚠ No API key for %s. Use /key to set one]\n" choice)))))

  (lin-agent-slash-register "/retry" "Retry the last user message"
    (lambda (_args)
      (let ((last-user-msg
             (cl-find-if (lambda (m) (equal (alist-get 'role m) "user"))
                         (reverse lin-agent--messages))))
        (if last-user-msg
            (let ((content (alist-get 'content last-user-msg)))
              (when (vectorp content)
                (setq content (alist-get 'text (cl-find-if
                                                (lambda (b) (equal (alist-get 'type b) "text"))
                                                (append content nil)))))
              (when (stringp content)
                ;; Remove last assistant+user pair
                (when (>= (length lin-agent--messages) 2)
                  (setq lin-agent--messages (butlast lin-agent--messages 2))
                  (cl-decf lin-agent--turn-count))
                (lin-agent--display "\n[Retrying: %s]\n" (truncate-string-to-width content 60 nil nil "..."))
                (lin-agent/send-message content)))
          (lin-agent--display "\n[No message to retry]\n")))))

  (lin-agent-slash-register "/pr" "Generate a pull request description from current branch"
    (lambda (args)
      (let* ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                       default-directory))
             (base (or (car args) "main"))
             (branch (string-trim (shell-command-to-string
                                   (format "git -C %s rev-parse --abbrev-ref HEAD 2>/dev/null" (shell-quote-argument root)))))
             (log (shell-command-to-string
                   (format "git -C %s log %s..HEAD --oneline 2>/dev/null" (shell-quote-argument root) base)))
             (stat (shell-command-to-string
                    (format "git -C %s diff %s...HEAD --stat 2>/dev/null | head -40" (shell-quote-argument root) base))))
        (if (string-empty-p log)
            (lin-agent--display "\n[No commits ahead of %s]\n" base)
          (lin-agent/send-message
           (format "Generate a pull request description for branch '%s' (base: %s).

Commits:
%s

Changed files:
%s

Write a clear PR title + body with: Summary, Changes, Testing notes." branch base log stat))))))

  (lin-agent-slash-register "/review" "Ask LLM to review recent changes in project"
    (lambda (_args)
      (let* ((default-directory (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                                    default-directory))
             (diff (shell-command-to-string "git diff HEAD~1 --stat 2>/dev/null")))
        (if (string-empty-p diff)
            (lin-agent--display "\n[No recent git changes to review]\n")
          (lin-agent/send-message
           (format "Please review these recent git changes and provide feedback:\n\n%s"
                   (shell-command-to-string "git diff HEAD~1 2>/dev/null | head -200"))))))))

(defun lin-agent--try-slash-command (input)
  "If INPUT starts with /, dispatch to slash command. Return t if handled, nil otherwise."
  (when (string-prefix-p "/" input)
    (let* ((parts (split-string (string-trim input)))
           (cmd-name (car parts))
           (args (cdr parts))
           (entry (assoc cmd-name lin-agent--slash-commands)))
      (if entry
          (progn
            (funcall (plist-get (cdr entry) :handler) args)
            t)
        (lin-agent--display "\n[Unknown command: %s. Try /help]\n" cmd-name)
        t))))

;; ==================== Transient Command Palette ====================

(defun lin-agent--define-transient ()
  "Define the Claude Agent transient command palette."
  (transient-define-prefix lin-agent-command-palette ()
    "Claude Agent Commands"
    ["Session"
     ("a" "Start session"    lin-agent/start-session)
     ("s" "Send message"     lin-agent/send-with-history)
     ("q" "Stop session"     lin-agent/stop)
     ("x" "Exit & close buf" (lambda () (interactive)
                              (if (eq major-mode 'lin-agent-mode)
                                  (lin-agent/close-session)
                                (user-error "Run from the Claude Agent buffer"))))
     ("S" "Save session"     lin-agent/save-session)
     ("R" "Resume session"   lin-agent/resume-session)
     ("N" "Rename session"   lin-agent/rename-session)
     ("y" "Copy response"    lin-agent/copy-last-response)]
    ["Mode"
     ("1" "Agent mode"       (lambda () (interactive) (lin-agent/set-mode 'agent)))
     ("2" "Plan mode"        (lambda () (interactive) (lin-agent/set-mode 'plan)))
     ("3" "Ask mode"         (lambda () (interactive) (lin-agent/set-mode 'ask)))]
    ["Memory"
     ("m" "Search memory"    lin-agent/memory-search)
     ("w" "Store memory"     lin-agent/memory-store)
     ("M" "Connect MCP"      lin-agent/memory-connect)]
    ["Context & Debug"
     ("c" "Compact context"  lin-agent/compact-context)
     ("C" "LLM compact"      (lambda () (interactive) (lin-agent/compact-context t)))
     ("i" "Inspect context"  lin-agent/inspect-context)
     ("r" "Rewind N turns"   lin-agent/rewind)
     ("t" "List tools"       lin-agent/list-tools)
     ("k" "List skills"      lin-agent/list-skills)
     ("$" "Show cost"        lin-agent/show-cost)
     ("B" "Token budget"     lin-agent/show-token-budget)
     ("d" "Doctor"           (lambda () (interactive) (lin-agent--try-slash-command "/doctor")))
     ("v" "Version"          (lambda () (interactive) (lin-agent--try-slash-command "/version")))
     ("p" "Switch provider"  lin-agent/switch-provider)]))

(provide 'lin-agent-commands)
;;; commands.el ends here
