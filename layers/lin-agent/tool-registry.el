;;; tool-registry.el --- Tool registration and built-in tools. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Aligns with Claude Code's Tool.ts + tools.ts: register, schema, execute

(require 'cl-lib)
(require 'json)

;; ==================== Jupyter (.ipynb) helpers (NotebookEditTool parity) ====================

(defun lin-agent--notebook-cells-list (nb)
  "Return notebook cells as a list."
  (let ((c (alist-get 'cells nb)))
    (cond
     ((vectorp c) (append c nil))
     ((listp c) c)
     (t nil))))

(defun lin-agent--notebook-set-cells (nb cells)
  "Set NB cells to CELLS, preserving vector vs list if possible."
  (let ((raw (alist-get 'cells nb)))
    (setf (alist-get 'cells nb)
          (if (vectorp raw) (vconcat cells) cells))))

(defun lin-agent--notebook-cell-source-string (cell)
  "Return source of CELL as a string."
  (let ((s (alist-get 'source cell)))
    (cond
     ((stringp s) s)
     ((listp s) (apply #'concat s))
     ((vectorp s) (mapconcat #'identity (append s nil) ""))
     (t ""))))

(defun lin-agent--json-read-file (path)
  "Parse JSON file PATH into an alist."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (if (fboundp 'json-parse-buffer)
        (json-parse-buffer :object-type 'alist :array-type 'list)
      (let ((json-object-type 'alist)
            (json-array-type 'list))
        (goto-char (point-min))
        (json-read)))))

(defun lin-agent--json-write-file (path obj)
  "Write OBJ as JSON to PATH."
  (with-temp-file path
    (insert (json-encode obj))))

;; ==================== Tool Registry ====================

(defvar lin-agent--tools (make-hash-table :test 'equal)
  "Hash table of registered tools. Key: name (string), Value: plist.")

(defun lin-agent-tool-register (name description input-schema handler &optional read-only)
  "Register a tool with NAME, DESCRIPTION, INPUT-SCHEMA, and HANDLER function.
If READ-ONLY is non-nil, the tool is marked as non-destructive."
  (puthash name
           (list :name name
                 :description description
                 :input-schema input-schema
                 :handler handler
                 :read-only read-only)
           lin-agent--tools))

(defun lin-agent-tool-get-handler (name)
  "Get handler function for tool NAME."
  (plist-get (gethash name lin-agent--tools) :handler))

(defun lin-agent-tool-definitions ()
  "Return tool definitions list formatted for Claude API."
  (let (defs)
    (maphash
     (lambda (_name tool)
       (push `(("name" . ,(plist-get tool :name))
               ("description" . ,(plist-get tool :description))
               ("input_schema" . ,(plist-get tool :input-schema)))
             defs))
     lin-agent--tools)
    (nreverse defs)))

;; ==================== Built-in Tools (reuse Emacs native) ====================

(defun lin-agent--init-builtin-tools ()
  "Register all built-in tools."

  (lin-agent-tool-register
   "file_read" "Read a file with line numbers. Supports offset/limit for large files."
   '(("type" . "object")
     ("properties" . (("path" . (("type" . "string") ("description" . "Absolute file path")))
                      ("offset" . (("type" . "integer") ("description" . "Start line (1-based, optional)")))
                      ("limit" . (("type" . "integer") ("description" . "Number of lines to read (optional)")))))
     ("required" . ["path"]))
   (lambda (input)
     (let ((path (alist-get 'path input))
           (offset (alist-get 'offset input))
           (limit (alist-get 'limit input)))
       (if (file-exists-p path)
           (with-temp-buffer
             (insert-file-contents path)
             (let* ((lines (split-string (buffer-string) "\n"))
                    (total (length lines))
                    (start (max 0 (1- (or offset 1))))
                    (end (if limit (min total (+ start limit)) total))
                    (selected (cl-subseq lines start end))
                    (width (length (number-to-string end))))
               (concat (format "File: %s (%d lines total)\n" path total)
                       (mapconcat
                        (lambda (line)
                          (cl-incf start)
                          (format "%s|%s" (string-pad (number-to-string start) width nil t) line))
                        selected "\n"))))
         (format "File not found: %s" path))))
   t)

  (lin-agent-tool-register
   "file_write" "Write content to a file. Creates parent directories if needed."
   '(("type" . "object")
     ("properties" . (("path" . (("type" . "string") ("description" . "Absolute file path")))
                      ("content" . (("type" . "string") ("description" . "File content to write")))))
     ("required" . ["path" "content"]))
   (lambda (input)
     (let ((path (alist-get 'path input))
           (content (alist-get 'content input)))
       (let ((dir (file-name-directory path)))
         (when (and dir (not (file-directory-p dir)))
           (make-directory dir t)))
       (with-temp-file path
         (insert content))
       (format "Written %d bytes to %s" (length content) path))))

  (lin-agent-tool-register
   "file_edit" "Replace a string in a file. Fails if old_string matches 0 or >1 times (uniqueness check)."
   '(("type" . "object")
     ("properties" . (("path" . (("type" . "string") ("description" . "Absolute file path")))
                      ("old_string" . (("type" . "string") ("description" . "Exact string to find (must be unique)")))
                      ("new_string" . (("type" . "string") ("description" . "Replacement string")))))
     ("required" . ["path" "old_string" "new_string"]))
   (lambda (input)
     (let ((path (alist-get 'path input))
           (old-str (alist-get 'old_string input))
           (new-str (alist-get 'new_string input)))
       (with-temp-buffer
         (insert-file-contents path)
         (let ((count 0))
           (goto-char (point-min))
           (while (search-forward old-str nil t)
             (cl-incf count))
           (cond
            ((= count 0) (format "String not found in %s" path))
            ((> count 1) (format "String matches %d times in %s. Provide more context to make it unique." count path))
            (t (goto-char (point-min))
               (search-forward old-str nil t)
               (replace-match new-str t t)
               (write-region (point-min) (point-max) path)
               (format "Replaced in %s" path))))))))

  (lin-agent-tool-register
   "shell_exec" "Execute a shell command and return output. Times out after 120 seconds."
   '(("type" . "object")
     ("properties" . (("command" . (("type" . "string") ("description" . "Shell command to execute")))
                      ("working_directory" . (("type" . "string") ("description" . "Working directory (optional)")))
                      ("timeout" . (("type" . "integer") ("description" . "Timeout in seconds (default 120, max 300)")))))
     ("required" . ["command"]))
   (lambda (input)
     (let* ((cmd (alist-get 'command input))
            (dir (or (alist-get 'working_directory input) default-directory))
            (timeout (min 300 (or (alist-get 'timeout input) 120)))
            (default-directory dir))
       (with-temp-buffer
         (let ((exit-code (call-process-region nil nil shell-file-name nil t nil
                                               shell-command-switch
                                               (format "timeout %d %s" timeout cmd))))
           (format "[exit %s]\n%s" exit-code (buffer-string)))))))

  (lin-agent-tool-register
   "grep_search" "Search for a regex pattern in files using ripgrep. Caps at 200 matches."
   '(("type" . "object")
     ("properties" . (("pattern" . (("type" . "string") ("description" . "Regex pattern to search")))
                      ("path" . (("type" . "string") ("description" . "Directory or file to search in")))
                      ("glob" . (("type" . "string") ("description" . "File glob filter (e.g. \"*.el\")")))))
     ("required" . ["pattern"]))
   (lambda (input)
     (let* ((pattern (alist-get 'pattern input))
            (path (or (alist-get 'path input) default-directory))
            (glob (alist-get 'glob input))
            (cmd (format "rg --no-heading --line-number --max-count 200 %s %s -- %s 2>&1"
                         (if glob (format "--glob '%s'" glob) "")
                         (shell-quote-argument pattern)
                         (shell-quote-argument path))))
       (shell-command-to-string cmd)))
   t)

  (lin-agent-tool-register
   "glob_find" "Find files matching a glob pattern. Supports ** for recursive matching."
   '(("type" . "object")
     ("properties" . (("pattern" . (("type" . "string") ("description" . "Glob pattern (e.g. \"**/*.el\", \"*.py\")")))
                      ("path" . (("type" . "string") ("description" . "Root directory to search")))))
     ("required" . ["pattern"]))
   (lambda (input)
     (let* ((pattern (alist-get 'pattern input))
            (path (or (alist-get 'path input) default-directory))
            (name-part (if (string-match "\\*\\*/\\(.+\\)" pattern)
                           (match-string 1 pattern)
                         pattern))
            (cmd (format "find %s -type f -name %s 2>/dev/null | head -100"
                         (shell-quote-argument path)
                         (shell-quote-argument name-part))))
       (shell-command-to-string cmd)))
   t)

  (lin-agent-tool-register
   "list_directory" "List files and directories at a path."
   '(("type" . "object")
     ("properties" . (("path" . (("type" . "string") ("description" . "Directory path to list")))))
     ("required" . ["path"]))
   (lambda (input)
     (let ((path (alist-get 'path input)))
       (if (file-directory-p path)
           (mapconcat #'identity (directory-files path nil nil t) "\n")
         (format "Not a directory: %s" path))))
   t)

  ;; ==================== Extended Tools (Claude Code parity) ====================

  (lin-agent-tool-register
   "todo_write" "Manage a structured task list. Create, update, or list TODO items."
   '(("type" . "object")
     ("properties" . (("action" . (("type" . "string") ("enum" . ["add" "update" "list" "clear"])
                                   ("description" . "Action: add, update, list, or clear")))
                      ("id" . (("type" . "string") ("description" . "Todo ID (for update)")))
                      ("content" . (("type" . "string") ("description" . "Todo description (for add)")))
                      ("status" . (("type" . "string") ("enum" . ["pending" "in_progress" "completed" "cancelled"])
                                   ("description" . "Status (for add/update)")))))
     ("required" . ["action"]))
   (lambda (input)
     (let ((action (alist-get 'action input)))
       (cond
        ((equal action "add")
         (let ((id (or (alist-get 'id input)
                       (format "t%d" (random 10000))))
               (content (or (alist-get 'content input) ""))
               (status (or (alist-get 'status input) "pending")))
           (push (list :id id :content content :status status)
                 lin-agent--todo-list)
           (format "Added: [%s] %s (%s)" id content status)))
        ((equal action "update")
         (let* ((id (alist-get 'id input))
                (item (cl-find id lin-agent--todo-list
                               :key (lambda (i) (plist-get i :id)) :test #'equal)))
           (if item
               (progn
                 (when-let ((s (alist-get 'status input)))
                   (plist-put item :status s))
                 (when-let ((c (alist-get 'content input)))
                   (plist-put item :content c))
                 (format "Updated: [%s] %s (%s)" id (plist-get item :content) (plist-get item :status)))
             (format "Todo not found: %s" id))))
        ((equal action "list")
         (if lin-agent--todo-list
             (mapconcat (lambda (item)
                          (format "[%s] %s — %s"
                                  (plist-get item :id)
                                  (plist-get item :content)
                                  (plist-get item :status)))
                        (reverse lin-agent--todo-list) "\n")
           "(no todos)"))
        ((equal action "clear")
         (setq lin-agent--todo-list nil)
         "Todos cleared.")
        (t (format "Unknown action: %s" action))))))

  (lin-agent-tool-register
   "web_fetch" "Fetch a URL and return its content as text. Useful for reading documentation."
   '(("type" . "object")
     ("properties" . (("url" . (("type" . "string") ("description" . "URL to fetch")))))
     ("required" . ["url"]))
   (lambda (input)
     (let ((url (alist-get 'url input)))
       (condition-case err
           (with-temp-buffer
             (url-insert-file-contents url)
             (let ((content (buffer-string)))
               (if (> (length content) 50000)
                   (substring content 0 50000)
                 content)))
         (error (format "Fetch failed: %s" (error-message-string err))))))
   t)

  (lin-agent-tool-register
   "ask_user" "Ask the user a question and wait for their response."
   '(("type" . "object")
     ("properties" . (("question" . (("type" . "string") ("description" . "Question to ask the user")))))
     ("required" . ["question"]))
   (lambda (input)
     (let ((question (alist-get 'question input)))
       (read-string (format "[Agent asks] %s\n> " question)))))

;; ==================== LSP Tools (eglot/lsp-mode) ====================

  (lin-agent-tool-register
   "lsp_definition" "Find the definition of a symbol using LSP (eglot or lsp-mode)."
   '(("type" . "object")
     ("properties" . (("file" . (("type" . "string") ("description" . "Absolute file path")))
                      ("line" . (("type" . "integer") ("description" . "Line number (1-based)")))
                      ("column" . (("type" . "integer") ("description" . "Column number (0-based)")))))
     ("required" . ["file" "line"]))
   (lambda (input)
     (let ((file (alist-get 'file input))
           (line (alist-get 'line input))
           (col (or (alist-get 'column input) 0)))
       (condition-case err
           (with-current-buffer (find-file-noselect file)
             (goto-char (point-min))
             (forward-line (1- line))
             (forward-char col)
             (cond
              ((fboundp 'xref-find-definitions)
               (let* ((id (xref-backend-identifier-at-point (xref-find-backend)))
                      (defs (xref-backend-definitions (xref-find-backend) id)))
                 (if defs
                     (mapconcat
                      (lambda (xref)
                        (let ((loc (xref-item-location xref)))
                          (format "%s:%d"
                                  (xref-location-group loc)
                                  (xref-location-line loc))))
                      defs "\n")
                   "No definitions found.")))
              (t "LSP not available.")))
         (error (format "LSP error: %s" (error-message-string err))))))
   t)

  (lin-agent-tool-register
   "lsp_references" "Find all references to a symbol using LSP."
   '(("type" . "object")
     ("properties" . (("file" . (("type" . "string") ("description" . "Absolute file path")))
                      ("line" . (("type" . "integer") ("description" . "Line number (1-based)")))
                      ("column" . (("type" . "integer") ("description" . "Column number (0-based)")))))
     ("required" . ["file" "line"]))
   (lambda (input)
     (let ((file (alist-get 'file input))
           (line (alist-get 'line input))
           (col (or (alist-get 'column input) 0)))
       (condition-case err
           (with-current-buffer (find-file-noselect file)
             (goto-char (point-min))
             (forward-line (1- line))
             (forward-char col)
             (cond
              ((fboundp 'xref-find-references)
               (let* ((id (xref-backend-identifier-at-point (xref-find-backend)))
                      (refs (xref-backend-references (xref-find-backend) id)))
                 (if refs
                     (mapconcat
                      (lambda (xref)
                        (let ((loc (xref-item-location xref)))
                          (format "%s:%d: %s"
                                  (xref-location-group loc)
                                  (xref-location-line loc)
                                  (xref-item-summary xref))))
                      (cl-subseq refs 0 (min 50 (length refs))) "\n")
                   "No references found.")))
              (t "LSP not available.")))
         (error (format "LSP error: %s" (error-message-string err))))))
   t)

  (lin-agent-tool-register
   "lsp_symbols" "List symbols in a file or workspace using LSP."
   '(("type" . "object")
     ("properties" . (("file" . (("type" . "string") ("description" . "File to list symbols from")))
                      ("query" . (("type" . "string") ("description" . "Symbol name pattern to search (optional)")))))
     ("required" . ["file"]))
   (lambda (input)
     (let ((file (alist-get 'file input)))
       (condition-case err
           (with-current-buffer (find-file-noselect file)
             (if (fboundp 'imenu--make-index-alist)
                 (let ((index (ignore-errors (imenu--make-index-alist t))))
                   (if index
                       (lin-agent--flatten-imenu index "")
                     "No symbols found."))
               "imenu not available."))
         (error (format "Symbol error: %s" (error-message-string err))))))
   t)

;; ==================== Mode Switch Tools (LLM-invokable) ====================

  (lin-agent-tool-register
   "enter_plan_mode" "Switch the agent to PLAN mode (read-only analysis, no file modifications)."
   '(("type" . "object")
     ("properties" . (("reason" . (("type" . "string") ("description" . "Why switching to plan mode")))))
     ("required" . []))
   (lambda (input)
     (setq lin-agent--mode 'plan)
     (format "Switched to PLAN mode. %s"
             (or (alist-get 'reason input) "Read-only analysis active."))))

  (lin-agent-tool-register
   "exit_plan_mode" "Switch the agent back to AGENT mode (full tool access)."
   '(("type" . "object")
     ("properties" . (("summary" . (("type" . "string") ("description" . "Summary of the plan")))))
     ("required" . []))
   (lambda (input)
     (setq lin-agent--mode 'agent)
     (format "Switched to AGENT mode. Plan summary: %s"
             (or (alist-get 'summary input) "Ready to execute."))))

;; ==================== Config Tool (LLM self-tuning) ====================

  (lin-agent-tool-register
   "config" "Get or set agent configuration. Actions: get, set, list."
   '(("type" . "object")
     ("properties" . (("action" . (("type" . "string") ("enum" . ["get" "set" "list"])
                                   ("description" . "get, set, or list")))
                      ("key" . (("type" . "string") ("description" . "Config key (e.g. model, max_tokens, mode)")))
                      ("value" . (("type" . "string") ("description" . "Value to set")))))
     ("required" . ["action"]))
   (lambda (input)
     (let ((action (alist-get 'action input))
           (key (alist-get 'key input))
           (value (alist-get 'value input)))
       (cond
        ((equal action "list")
         (format "model: %s\nmax_tokens: %d\nmode: %s\nauto_memory: %s\nauto_compact_threshold: %d\napi_version: %s"
                 lin-agent-model lin-agent-max-tokens lin-agent--mode
                 lin-agent-auto-memory lin-agent-auto-compact-threshold lin-agent-api-version))
        ((equal action "get")
         (pcase key
           ("model" lin-agent-model)
           ("max_tokens" (number-to-string lin-agent-max-tokens))
           ("mode" (symbol-name lin-agent--mode))
           ("auto_memory" (if lin-agent-auto-memory "true" "false"))
           (_ (format "Unknown key: %s" key))))
        ((equal action "set")
         (pcase key
           ("model" (setq lin-agent-model value) (format "model → %s" value))
           ("max_tokens" (setq lin-agent-max-tokens (string-to-number value))
            (format "max_tokens → %d" lin-agent-max-tokens))
           ("mode" (setq lin-agent--mode (intern value)) (format "mode → %s" value))
           ("auto_memory" (setq lin-agent-auto-memory (equal value "true"))
            (format "auto_memory → %s" lin-agent-auto-memory))
           (_ (format "Unknown or read-only key: %s" key))))
        (t (format "Unknown action: %s" action))))))

;; ==================== WebSearch Tool ====================

  (lin-agent-tool-register
   "web_search" "Search the web using DuckDuckGo. Returns search results."
   '(("type" . "object")
     ("properties" . (("query" . (("type" . "string") ("description" . "Search query")))))
     ("required" . ["query"]))
   (lambda (input)
     (let ((query (alist-get 'query input)))
       (condition-case err
           (with-temp-buffer
             (url-insert-file-contents
              (format "https://html.duckduckgo.com/html/?q=%s" (url-hexify-string query)))
             (let ((results nil) (count 0))
               (goto-char (point-min))
               (while (and (< count 10)
                           (re-search-forward
                            "class=\"result__a\"[^>]*href=\"\\([^\"]+\\)\"[^>]*>\\([^<]+\\)" nil t))
                 (push (format "%d. %s\n   %s"
                               (cl-incf count)
                               (match-string 2)
                               (match-string 1))
                       results))
               (if results
                   (mapconcat #'identity (nreverse results) "\n")
                 (format "No results for: %s" query))))
         (error (format "Search failed: %s" (error-message-string err))))))
   t)

;; ==================== Sleep Tool ====================

  (lin-agent-tool-register
   "sleep" "Wait for a specified number of seconds."
   '(("type" . "object")
     ("properties" . (("seconds" . (("type" . "integer") ("description" . "Seconds to wait (max 60)")))))
     ("required" . ["seconds"]))
   (lambda (input)
     (let ((seconds (min 60 (or (alist-get 'seconds input) 1))))
       (sleep-for seconds)
       (format "Waited %d seconds." seconds))))

;; ==================== Agent (Subagent) Tool ====================

  (lin-agent-tool-register
   "agent" "Spawn a subagent to handle a self-contained task independently.
The subagent gets its own conversation context and can use tools.
Use for parallel subtasks, research, or isolated work."
   '(("type" . "object")
     ("properties" . (("task" . (("type" . "string")
                                 ("description" . "Detailed task description for the subagent")))
                      ("mode" . (("type" . "string")
                                 ("enum" . ["agent" "ask"])
                                 ("description" . "Subagent mode: agent (can write) or ask (read-only)")))))
     ("required" . ["task"]))
   (lambda (input)
     (let* ((task (alist-get 'task input))
            (mode (or (alist-get 'mode input) "ask"))
            (sub-tools (if (equal mode "ask")
                           (lin-agent--readonly-tool-definitions-internal)
                         (lin-agent-tool-definitions)))
            (sub-messages (list (lin-agent--make-user-msg task)))
            (response (lin-agent--call-api sub-messages sub-tools)))
       (lin-agent--extract-text response))))

;; ==================== Enhanced Diff Tool ====================

  (lin-agent-tool-register
   "git_diff" "Show git diff with various options."
   '(("type" . "object")
     ("properties" . (("target" . (("type" . "string")
                                    ("description" . "Diff target: staged, HEAD, HEAD~N, branch name, or commit hash. Default: HEAD")))
                      ("path" . (("type" . "string")
                                 ("description" . "Optional file path to restrict diff")))
                      ("stat_only" . (("type" . "boolean")
                                      ("description" . "If true, show only stat summary")))))
     ("required" . []))
   (lambda (input)
     (let* ((target (or (alist-get 'target input) "HEAD"))
            (path (alist-get 'path input))
            (stat-only (alist-get 'stat_only input))
            (root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                      default-directory))
            (cmd (format "git -C %s diff %s%s%s 2>&1 | head -500"
                         (shell-quote-argument root)
                         (if (equal target "staged") "--cached" target)
                         (if stat-only " --stat" "")
                         (if path (format " -- %s" (shell-quote-argument path)) ""))))
       (let ((output (shell-command-to-string cmd)))
         (if (string-empty-p (string-trim output))
             "No differences found."
           output))))
   t)

  (lin-agent-tool-register
   "git_worktree" "List git worktrees for the current repo (Claude Code EnterWorktree/ExitWorktree parity for discovery)."
   '(("type" . "object")
     ("properties" . (("path" . (("type" . "string")
                                   ("description" . "Optional repository root (default: project root)")))))
     ("required" . []))
   (lambda (input)
     (let* ((root (or (alist-get 'path input)
                      (and (fboundp 'projectile-project-root) (projectile-project-root))
                      default-directory))
            (cmd (format "git -C %s worktree list 2>&1" (shell-quote-argument (expand-file-name root)))))
       (shell-command-to-string cmd)))
   t)

  (lin-agent-tool-register
   "mcp_list_resources" "List MCP resources from one connected server or all servers (Claude Code ListMcpResourcesTool parity)."
   '(("type" . "object")
     ("properties" . (("server" . (("type" . "string")
                                    ("description" . "MCP server name from lin-agent-mcp-server-configs; omit to list all servers")))))
     ("required" . []))
   (lambda (input)
     (require 'mcp-client nil t)
     (if (not (fboundp 'lin-agent-mcp-list-resources-all-formatted))
         "mcp-client.el not loaded."
       (let ((server (alist-get 'server input)))
         (if (and server (not (string-empty-p server)))
             (condition-case err
                 (let ((res (lin-agent-mcp-list-resources server)))
                   (if res
                       (mapconcat (lambda (r)
                                    (format "%s  %s"
                                            (or (alist-get 'uri r) (alist-get 'name r) "?")
                                            (or (alist-get 'name r) "")))
                                  (if (vectorp res) (append res nil) res) "\n")
                     "(no resources)"))
               (error (format "mcp_list_resources: %s" (error-message-string err))))
           (lin-agent-mcp-list-resources-all-formatted)))))
   t)

  (lin-agent-tool-register
   "mcp_read_resource" "Read a resource by URI from a named MCP server (Claude Code ReadMcpResourceTool parity)."
   '(("type" . "object")
     ("properties" . (("server" . (("type" . "string") ("description" . "Connected MCP server name")))
                      ("uri" . (("type" . "string") ("description" . "Resource URI from mcp_list_resources")))))
     ("required" . ["server" "uri"]))
   (lambda (input)
     (require 'mcp-client nil t)
     (if (not (fboundp 'lin-agent-mcp-read-resource-sync))
         "mcp-client.el not loaded."
       (let ((server (alist-get 'server input))
             (uri (alist-get 'uri input)))
         (lin-agent-mcp-read-resource-sync server uri))))
   t)

  (lin-agent-tool-register
   "notebook_edit" "Edit a Jupyter notebook (.ipynb): list cells, replace, insert, or delete (NotebookEditTool parity, nbformat 4)."
   '(("type" . "object")
     ("properties" . (("notebook_path" . (("type" . "string") ("description" . "Absolute path to .ipynb file")))
                      ("action" . (("type" . "string")
                                   ("enum" . ["list" "replace" "insert" "delete"])
                                   ("description" . "list: show cells; replace/insert/delete: need cell_index")))
                      ("cell_index" . (("type" . "integer") ("description" . "0-based cell index (for replace/delete; for insert: insert after this index, use -1 for top)")))
                      ("new_source" . (("type" . "string") ("description" . "New cell source (replace/insert)")))
                      ("cell_type" . (("type" . "string")
                                      ("enum" . ["code" "markdown"])
                                      ("description" . "For insert only; default code")))))
     ("required" . ["notebook_path" "action"]))
   (lambda (input)
     (let* ((path (expand-file-name (alist-get 'notebook_path input)))
            (action (alist-get 'action input))
            (idx-raw (alist-get 'cell_index input))
            (cell-index (if idx-raw (truncate (float idx-raw)) nil))
            (new-src (alist-get 'new_source input))
            (ctype (or (alist-get 'cell_type input) "code")))
       (unless (string-suffix-p ".ipynb" path)
         (error "notebook_edit: path must be .ipynb"))
       (unless (file-exists-p path)
         (error "notebook_edit: file not found: %s" path))
       (let ((nb (lin-agent--json-read-file path)))
         (unless (alist-get 'cells nb)
           (error "notebook_edit: invalid notebook (no cells)"))
         (pcase action
           ("list"
            (let ((cells (lin-agent--notebook-cells-list nb))
                  (i 0))
              (mapconcat
               (lambda (cell)
                 (prog1
                     (format "[%d] %s  %s"
                             i
                             (or (alist-get 'cell_type cell) "?")
                             (truncate-string-to-width
                              (replace-regexp-in-string "\n" " " (lin-agent--notebook-cell-source-string cell))
                              72 nil nil "..."))
                   (cl-incf i)))
               cells "\n")))
           ("replace"
            (unless cell-index (error "notebook_edit: cell_index required"))
            (unless new-src (error "notebook_edit: new_source required"))
            (let* ((cells (lin-agent--notebook-cells-list nb))
                   (cell (nth cell-index cells)))
              (unless cell (error "notebook_edit: no cell at index %s" cell-index))
              (setf (alist-get 'source cell) new-src)
              (lin-agent--notebook-set-cells nb cells)
              (lin-agent--json-write-file path nb)
              (format "Replaced cell %d in %s" cell-index path)))
           ("delete"
            (unless cell-index (error "notebook_edit: cell_index required"))
            (let* ((cells (lin-agent--notebook-cells-list nb))
                   (n (length cells)))
              (when (or (< cell-index 0) (>= cell-index n))
                (error "notebook_edit: cell_index out of range"))
              (let ((new (append (cl-subseq cells 0 cell-index)
                                 (cl-subseq cells (1+ cell-index)))))
                (lin-agent--notebook-set-cells nb new)
                (lin-agent--json-write-file path nb)
                (format "Deleted cell %d from %s" cell-index path))))
           ("insert"
            (unless new-src (error "notebook_edit: new_source required"))
            (let* ((cells (lin-agent--notebook-cells-list nb))
                   (pos (cond
                         ((and cell-index (= cell-index -1)) 0)
                         ((and cell-index (>= cell-index 0)) (1+ cell-index))
                         (t 0)))
                   (new-cell (if (member (downcase (format "%s" ctype)) '("markdown" "md"))
                                 `((cell_type . "markdown") (metadata . ()) (source . ,new-src))
                               `((cell_type . "code") (metadata . ()) (outputs . ,(vector)) (source . ,new-src))))
                   (new (append (cl-subseq cells 0 pos)
                                (list new-cell)
                                (cl-subseq cells pos))))
              (lin-agent--notebook-set-cells nb new)
              (lin-agent--json-write-file path nb)
              (format "Inserted %s cell at position %d in %s" ctype pos path)))
           (_ (error "notebook_edit: unknown action %s" action)))))))
   nil)

  (lin-agent-tool-register
   "task_create" "Create a structured task with title, description, and initial status."
   '(("type" . "object")
     ("properties" . (("title" . (("type" . "string") ("description" . "Task title")))
                      ("description" . (("type" . "string") ("description" . "Detailed task description")))
                      ("status" . (("type" . "string") ("enum" . ["pending" "in_progress"])
                                   ("description" . "Initial status (default: pending)")))))
     ("required" . ["title"]))
   (lambda (input)
     (let* ((id (format "task-%04d" (1+ (length lin-agent--task-list))))
            (title (alist-get 'title input))
            (desc (or (alist-get 'description input) ""))
            (status (or (alist-get 'status input) "pending"))
            (now (format-time-string "%Y-%m-%d %H:%M")))
       (push (list :id id :title title :description desc
                   :status status :created now :updated now)
             lin-agent--task-list)
       (format "Created: [%s] %s (%s)" id title status))))

  (lin-agent-tool-register
   "task_update" "Update a task's status or description."
   '(("type" . "object")
     ("properties" . (("id" . (("type" . "string") ("description" . "Task ID")))
                      ("status" . (("type" . "string")
                                   ("enum" . ["pending" "in_progress" "completed" "cancelled"])
                                   ("description" . "New status")))
                      ("description" . (("type" . "string") ("description" . "Updated description")))))
     ("required" . ["id"]))
   (lambda (input)
     (let* ((id (alist-get 'id input))
            (task (cl-find id lin-agent--task-list
                           :key (lambda (item) (plist-get item :id)) :test #'equal)))
       (if task
           (progn
             (when-let ((s (alist-get 'status input)))
               (plist-put task :status s))
             (when-let ((d (alist-get 'description input)))
               (plist-put task :description d))
             (plist-put task :updated (format-time-string "%Y-%m-%d %H:%M"))
             (format "Updated: [%s] %s (%s)" id (plist-get task :title) (plist-get task :status)))
         (format "Task not found: %s" id)))))

  (lin-agent-tool-register
   "task_list" "List all tasks, optionally filtered by status."
   '(("type" . "object")
     ("properties" . (("status" . (("type" . "string")
                                    ("enum" . ["pending" "in_progress" "completed" "cancelled" "all"])
                                    ("description" . "Filter by status (default: all)")))))
     ("required" . []))
   (lambda (input)
     (let* ((filter-status (alist-get 'status input))
            (tasks (if (or (null filter-status) (equal filter-status "all"))
                       lin-agent--task-list
                     (cl-remove-if-not
                      (lambda (item) (equal (plist-get item :status) filter-status))
                      lin-agent--task-list))))
       (if tasks
           (mapconcat
            (lambda (item)
              (format "[%s] %s — %s\n    %s\n    Created: %s | Updated: %s"
                      (plist-get item :id)
                      (plist-get item :title)
                      (plist-get item :status)
                      (plist-get item :description)
                      (plist-get item :created)
                      (plist-get item :updated)))
            (reverse tasks) "\n\n")
         "(no tasks)")))
   t)

  (lin-agent-tool-register
   "task_get" "Get details of a specific task by ID."
   '(("type" . "object")
     ("properties" . (("id" . (("type" . "string") ("description" . "Task ID")))))
     ("required" . ["id"]))
   (lambda (input)
     (let* ((id (alist-get 'id input))
            (task (cl-find id lin-agent--task-list
                           :key (lambda (item) (plist-get item :id)) :test #'equal)))
       (if task
           (format "ID: %s\nTitle: %s\nStatus: %s\nDescription: %s\nCreated: %s\nUpdated: %s"
                   (plist-get task :id)
                   (plist-get task :title)
                   (plist-get task :status)
                   (plist-get task :description)
                   (plist-get task :created)
                   (plist-get task :updated))
         (format "Task not found: %s" id))))
   t)

  ;; ==================== Tool Search ====================

  (lin-agent-tool-register
   "tool_search" "Search available tools by keyword in name or description."
   '(("type" . "object")
     ("properties" . (("query" . (("type" . "string") ("description" . "Search keyword")))))
     ("required" . ["query"]))
   (lambda (input)
     (let* ((query (downcase (alist-get 'query input)))
            (matches nil))
       (maphash
        (lambda (name tool)
          (when (or (string-match-p query (downcase name))
                    (string-match-p query (downcase (or (plist-get tool :description) ""))))
            (push (format "%-20s %s%s"
                          name
                          (plist-get tool :description)
                          (if (plist-get tool :read-only) " [readonly]" ""))
                  matches)))
        lin-agent--tools)
       (if matches
           (mapconcat #'identity (nreverse matches) "\n")
         (format "No tools matching '%s'" (alist-get 'query input)))))
   t)

  ;; ==================== IDE Integration ====================

  (lin-agent-tool-register
   "open_file" "Open a file in Emacs and optionally go to a line."
   '(("type" . "object")
     ("properties" . (("path" . (("type" . "string") ("description" . "File path")))
                      ("line" . (("type" . "integer") ("description" . "Line number to go to (optional)")))))
     ("required" . ["path"]))
   (lambda (input)
     (let ((path (expand-file-name (alist-get 'path input)))
           (line (alist-get 'line input)))
       (if (file-exists-p path)
           (progn
             (find-file-noselect path)
             (when line
               (with-current-buffer (find-buffer-visiting path)
                 (goto-char (point-min))
                 (forward-line (1- line))))
             (format "Opened %s%s" path (if line (format " at line %d" line) "")))
         (format "File not found: %s" path))))

  (lin-agent-tool-register
   "project_structure" "Get project directory structure (tree view, limited depth)."
   '(("type" . "object")
     ("properties" . (("path" . (("type" . "string") ("description" . "Root directory path")))
                      ("depth" . (("type" . "integer") ("description" . "Max depth (default 3)")))))
     ("required" . ["path"]))
   (lambda (input)
     (let* ((root (expand-file-name (alist-get 'path input)))
            (depth (or (alist-get 'depth input) 3))
            (cmd (format "find %s -maxdepth %d -not -path '*/\\.*' -not -path '*/node_modules/*' -not -path '*/vendor/*' | head -100 | sort"
                         (shell-quote-argument root) depth)))
       (if (file-directory-p root)
           (shell-command-to-string cmd)
         (format "Directory not found: %s" root))))
   t)

) ;; end of lin-agent--init-builtin-tools

;; ==================== Imenu helper for LSP symbols ====================

(defun lin-agent--readonly-tool-definitions-internal ()
  "Return only read-only tool definitions (internal helper)."
  (let (defs)
    (maphash
     (lambda (_name tool)
       (when (plist-get tool :read-only)
         (push `(("name" . ,(plist-get tool :name))
                 ("description" . ,(plist-get tool :description))
                 ("input_schema" . ,(plist-get tool :input-schema)))
               defs)))
     lin-agent--tools)
    (nreverse defs)))

(defun lin-agent--flatten-imenu (index prefix)
  "Flatten imenu INDEX into readable lines with PREFIX."
  (mapconcat
   (lambda (item)
     (cond
      ((imenu--subalist-p item)
       (lin-agent--flatten-imenu (cdr item) (concat prefix (car item) ".")))
      ((number-or-marker-p (cdr item))
       (format "%s%s (line %d)" prefix (car item)
               (with-current-buffer (marker-buffer (cdr item))
                 (line-number-at-pos (cdr item)))))
      (t (format "%s%s" prefix (car item)))))
   index "\n"))

(defvar lin-agent--todo-list nil
  "In-session todo list for the agent.")

(defvar lin-agent--task-list nil
  "Structured task list: list of plists (:id :title :status :description :created :updated).")

(provide 'tool-registry)
;;; tool-registry.el ends here
