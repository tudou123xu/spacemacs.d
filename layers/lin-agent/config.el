;;; config.el --- lin-agent layer configuration file for Spacemacs. -*- lexical-binding: t; -*-
;; Author: xuzhifeng

;; Ensure layer directory is on load-path for internal requires
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path dir))

;; Parity note (vs Claude Code @anthropic-ai/claude-code decompiled src):
;; Covered in Emacs: core file/shell/grep/glob/LSP, plan/ask modes, todos, tasks,
;; git diff/worktree, web fetch/search, memory (MemPalace MCP), MCP `resources/list`
;; + `resources/read`, notebook cell edit (.ipynb), generic MCP tool proxy, skills,
;; subagent `agent', config/sleep/ask_user, @mentions, slash commands.
;; Not ported: PowerShellTool, KAIROS Brief/Team/Task remote, browser automation,
;; feature-gated internal modules (see reference README).

(defconst lin-agent-version "0.4-claude-code-parity"
  "lin-agent layer version (informational).")

;; ==================== Agent Configuration ====================

(defvar lin-agent-provider 'siliconflow
  "Current AI provider symbol: claude, deepseek, siliconflow, openai.")

;; ==================== Built-in API Keys ====================

(defvar lin-agent-builtin-keys
  '((siliconflow . "sk-joikxozisukypzqwkbtfdrnsonykalzmojzjnbpwvumagglp")
    (deepseek    . "sk-1d92e11cbd284a439b78df6e7f4b280b"))
  "Built-in API keys for providers. Checked before authinfo and env vars.")

(defvar lin-agent-provider-configs
  '((claude      . (:model "claude-sonnet-4-20250514"
                    :endpoint "https://api.anthropic.com/v1/messages"
                    :api-type claude
                    :api-version "2023-06-01"))
    (deepseek    . (:model "deepseek-chat"
                    :endpoint "https://api.deepseek.com/v1/chat/completions"
                    :api-type openai))
    (siliconflow . (:model "deepseek-ai/DeepSeek-V3"
                    :endpoint "https://api.siliconflow.cn/v1/chat/completions"
                    :api-type openai))
    (openai      . (:model "gpt-4o"
                    :endpoint "https://api.openai.com/v1/chat/completions"
                    :api-type openai)))
  "Provider configurations. :api-type is either `claude' or `openai'.")

(defvar lin-agent-model nil
  "Override model name. If nil, uses provider default.")

(defvar lin-agent-max-tokens 8192
  "Maximum tokens per response.")

(defvar lin-agent-api-endpoint nil
  "Override API endpoint. If nil, uses provider default.")

(defvar lin-agent-api-version "2023-06-01"
  "Anthropic API version header (Claude only).")

(defun lin-agent--provider-config ()
  "Get current provider config plist."
  (or (alist-get lin-agent-provider lin-agent-provider-configs)
      (error "Unknown provider: %s" lin-agent-provider)))

(defun lin-agent--current-model ()
  "Get current model name."
  (or lin-agent-model
      (plist-get (lin-agent--provider-config) :model)))

(defun lin-agent--current-endpoint ()
  "Get current API endpoint."
  (or lin-agent-api-endpoint
      (plist-get (lin-agent--provider-config) :endpoint)))

(defun lin-agent--current-api-type ()
  "Get current API type: `claude' or `openai'."
  (plist-get (lin-agent--provider-config) :api-type))

(defvar lin-agent-memory-enabled t
  "Whether to enable MemPalace memory integration.")

(defvar lin-agent-auto-memory t
  "Whether to automatically write memories.
When non-nil:
- Session end: auto-write session summary to diary.
- Session start: auto-recall relevant memories into context.
- Per-turn: Claude is instructed to proactively use memory_store.")

(defvar lin-agent-auto-recall t
  "Whether to auto-search memory at session start and inject results.")

(defvar lin-agent-auto-compact-threshold 80000
  "Token count threshold to trigger automatic context compaction.")

(defvar lin-agent-system-prompt
  "You are a senior software architect embedded in Emacs. \
You have access to tools for file operations, shell commands, \
code search, and long-term memory. Be concise. Use tools proactively. \
Always confirm before destructive operations.

MEMORY RULES (follow strictly):
1. At the START of a task, use memory_search to check if you have prior \
knowledge about this topic, project, or file.
2. When you discover something important (a decision, a bug fix, an \
architecture pattern, a user preference), use memory_store to save it \
immediately — do NOT wait for the user to ask.
3. Categories for memory_store: wing = broad domain (e.g. \"project\", \
\"architecture\", \"debug\"), room = specific topic.
4. When you make a mistake and fix it, store the lesson learned.
5. Store user preferences (coding style, tool choices, naming conventions) \
when you observe them."
  "System prompt for the agent loop.")

;; ==================== Output Styles ====================

(defvar lin-agent-output-styles
  '((concise . "Be extremely concise. One-line answers when possible. No explanations unless asked.")
    (normal . "Balance thoroughness with brevity. Explain trade-offs.")
    (detailed . "Be thorough and detailed. Show reasoning steps. Include examples and alternatives.")
    (educational . "Explain concepts as if teaching. Include context, background, and rationale for decisions."))
  "Available output styles. Maps style symbol to prompt fragment.")

(defvar lin-agent-output-style 'normal
  "Current output style.")

;; ==================== Permission System ====================

(defvar lin-agent-permission-mode 'default
  "Global permission mode.
`default'      - use per-tool permissions below
`accept-edits' - auto-approve file writes/edits, still confirm shell
`bypass'       - auto-approve everything (dangerous)
`plan'         - read-only, no write tools allowed")

(defvar lin-agent-tool-permissions
  '((file_read . auto)
    (file_write . confirm)
    (file_edit . confirm)
    (shell_exec . confirm)
    (grep_search . auto)
    (glob_find . auto)
    (list_directory . auto)
    (memory_search . auto)
    (memory_store . auto)
    (memory_kg_query . auto)
    (todo_write . auto)
    (web_fetch . auto)
    (web_search . auto)
    (ask_user . auto)
    (lsp_definition . auto)
    (lsp_references . auto)
    (lsp_symbols . auto)
    (enter_plan_mode . auto)
    (exit_plan_mode . auto)
    (config . auto)
    (sleep . auto)
    (agent . confirm)
    (git_diff . auto)
    (git_worktree . auto)
    (task_create . confirm)
    (task_update . confirm)
    (task_list . auto)
    (task_get . auto)
    (tool_search . auto)
    (open_file . confirm)
    (project_structure . auto)
    (mcp_list_resources . auto)
    (mcp_read_resource . auto)
    (notebook_edit . confirm))
  "Tool permission rules. Keys must match tool names exactly (underscores).
`auto' = no confirmation, `confirm' = ask user.
Tools omitted here default to `confirm' in `agent-loop' (see `alist-get').")

(defvar lin-agent-shell-allowed-dirs nil
  "List of directory paths the shell_exec tool is allowed to operate in.
If nil, all directories are allowed. Paths must be absolute.")

(defvar lin-agent-shell-blocked-commands
  '("rm -rf /" "rm -rf /*" "mkfs" "dd if=" "> /dev/sd")
  "List of dangerous command patterns that are always blocked.")

;; ==================== Keybindings ====================

(spacemacs/set-leader-keys
  ;; Session
  "oca" #'lin-agent/start-session
  "ocs" #'lin-agent/send-message
  "ocq" #'lin-agent/stop
  ;; Modes
  "oc1" (lambda () (interactive) (lin-agent/set-mode 'agent))
  "oc2" (lambda () (interactive) (lin-agent/set-mode 'plan))
  "oc3" (lambda () (interactive) (lin-agent/set-mode 'ask))
  ;; Memory
  "ocm" #'lin-agent/memory-search
  "ocw" #'lin-agent/memory-store
  "ocM" #'lin-agent/toggle-auto-memory
  "ocC" #'lin-agent/memory-connect
  ;; Context & Tools
  "oct" #'lin-agent/list-tools
  "occ" #'lin-agent/compact-context
  "ocp" #'lin-agent/switch-provider
  ;; Skills
  "ock" #'lin-agent/list-skills
  ;; API Key
  "ocK" #'lin-ai/set-key
  ;; Session Management
  "ocS" #'lin-agent/save-session
  "ocR" #'lin-agent/resume-session
  ;; Cost
  "oc$" #'lin-agent/show-cost
  ;; Context Inspector & Rewind
  "oci" #'lin-agent/inspect-context
  "ocr" #'lin-agent/rewind
  "ocB" #'lin-agent/show-token-budget
  ;; Rename / Copy
  "ocn" #'lin-agent/rename-session
  "ocy" #'lin-agent/copy-last-response
  ;; Command Palette
  "oc." #'lin-agent-command-palette
  ;; Cursor-style @attach (project file/dir completion)
  "ocA" #'lin-agent-insert-mention
  ;; MCP disconnect
  "ocD" #'lin-agent/memory-disconnect)

;; ==================== User Hooks ====================

(defvar lin-agent-user-submit-hooks nil
  "Functions called before user input is processed. Each receives (prompt).
Return a modified prompt string, or nil to use original.")

(defvar lin-agent-user-stop-hooks nil
  "Functions called when session stops.")

(defvar lin-agent-turn-complete-hooks nil
  "Functions called after each assistant turn completes. Each receives (messages).")

;;; config.el ends here
