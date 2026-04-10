;;; memory-bridge.el --- MemPalace MCP integration for agent memory. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Bridges lin-agent to MemPalace MCP for long-term memory.

(require 'json)
(require 'cl-lib)
(require 'subr-x)

;; ==================== MCP Transport ====================

(defvar lin-agent-mcp--process nil
  "MemPalace MCP server process (stdio transport).")

(defvar lin-agent-mcp--request-id 0
  "JSON-RPC request ID counter.")

(defvar lin-agent-mcp--callbacks (make-hash-table)
  "Pending request callbacks.")

(defvar lin-agent-mcp-command nil
  "Command to start the MemPalace MCP server.
Set this to the actual command, e.g. '(\"npx\" \"mempalace-mcp\")
or '(\"python\" \"-m\" \"mempalace.server\").")

(defun lin-agent-mcp--next-id ()
  "Generate next JSON-RPC request ID."
  (cl-incf lin-agent-mcp--request-id))

(defun lin-agent-mcp--send (method params callback)
  "Send a JSON-RPC request to MemPalace with METHOD, PARAMS, and CALLBACK."
  (unless lin-agent-mcp--process
    (user-error "MemPalace MCP not connected. Run M-x lin-agent/memory-connect"))
  (let* ((id (lin-agent-mcp--next-id))
         (request `(("jsonrpc" . "2.0")
                    ("id" . ,id)
                    ("method" . ,method)
                    ("params" . ,params)))
         (json-str (concat (json-encode request) "\n")))
    (puthash id callback lin-agent-mcp--callbacks)
    (process-send-string lin-agent-mcp--process json-str)))

(defun lin-agent-mcp--filter (process output)
  "Handle output from MCP PROCESS."
  (dolist (line (split-string output "\n" t))
    (condition-case nil
        (let* ((json-object-type 'alist)
               (response (json-read-from-string line))
               (id (alist-get 'id response))
               (result (alist-get 'result response))
               (err (alist-get 'error response))
               (callback (gethash id lin-agent-mcp--callbacks)))
          (when callback
            (remhash id lin-agent-mcp--callbacks)
            (if err
                (funcall callback nil (alist-get 'message err))
              (funcall callback result nil))))
      (error nil))))

;; ==================== Connection Management ====================

(defun lin-agent/memory-connect ()
  "Connect to MemPalace MCP server."
  (interactive)
  (when lin-agent-mcp--process
    (delete-process lin-agent-mcp--process))
  (unless lin-agent-mcp-command
    (user-error "Set `lin-agent-mcp-command' to the MemPalace server command"))
  (setq lin-agent-mcp--process
        (make-process
         :name "mempalace-mcp"
         :command lin-agent-mcp-command
         :filter #'lin-agent-mcp--filter
         :sentinel (lambda (_proc event)
                     (message "MemPalace MCP: %s" (string-trim event)))
         :noquery t))
  (message "MemPalace MCP connected."))

(defun lin-agent/memory-disconnect ()
  "Disconnect from MemPalace MCP server."
  (interactive)
  (when lin-agent-mcp--process
    (delete-process lin-agent-mcp--process)
    (setq lin-agent-mcp--process nil)
    (message "MemPalace MCP disconnected.")))

;; ==================== Synchronous Wrappers for Tool Use ====================

(defun lin-agent-mcp--call-sync (method params &optional timeout)
  "Call MCP METHOD with PARAMS synchronously, waiting up to TIMEOUT seconds.
Signals `user-error' on timeout or MCP error."
  (unless lin-agent-mcp--process
    (user-error "MCP not connected. Call lin-agent/memory-connect first"))
  (let ((result nil) (err nil) (done nil))
    (lin-agent-mcp--send method params
      (lambda (r e) (setq result r err e done t)))
    (let ((deadline (+ (float-time) (or timeout 30))))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output lin-agent-mcp--process 0.1)))
    (unless done
      (user-error "MCP call timed out after %ds: %s" (or timeout 30) method))
    (when err (user-error "MCP error: %s" err))
    result))

;; ==================== Memory Tools (registered into agent) ====================

(defun lin-agent--init-memory-tools ()
  "Register memory tools into the agent tool registry."
  (when (and lin-agent-memory-enabled lin-agent-mcp--process)

    (lin-agent-tool-register
     "memory_search" "Search long-term memory for relevant knowledge."
     '(("type" . "object")
       ("properties" . (("query" . (("type" . "string") ("description" . "Search query")))
                        ("limit" . (("type" . "integer") ("description" . "Max results (default 5)")))))
       ("required" . ["query"]))
     (lambda (input)
       (let ((result (lin-agent-mcp--call-sync
                      "tools/call"
                      `(("name" . "mempalace_search")
                        ("arguments" . ,input)))))
         (or (alist-get 'content result) "No results found.")))
     t)

    (lin-agent-tool-register
     "memory_store" "Store important knowledge for future retrieval."
     '(("type" . "object")
       ("properties" . (("wing" . (("type" . "string") ("description" . "Knowledge category")))
                        ("room" . (("type" . "string") ("description" . "Sub-category")))
                        ("content" . (("type" . "string") ("description" . "Knowledge content to store")))))
       ("required" . ["wing" "room" "content"]))
     (lambda (input)
       (let ((result (lin-agent-mcp--call-sync
                      "tools/call"
                      `(("name" . "mempalace_add_drawer")
                        ("arguments" . ,(append input
                                                `(("source_file" . ,(or buffer-file-name "agent"))
                                                  ("added_by" . "lin-agent"))))))))
         (or (alist-get 'content result) "Stored."))))

    (lin-agent-tool-register
     "memory_kg_query" "Query knowledge graph relationships for an entity."
     '(("type" . "object")
       ("properties" . (("entity" . (("type" . "string") ("description" . "Entity name to query")))))
       ("required" . ["entity"]))
     (lambda (input)
       (let ((result (lin-agent-mcp--call-sync
                      "tools/call"
                      `(("name" . "mempalace_kg_query")
                        ("arguments" . ,(append input '(("direction" . "both"))))))))
         (or (alist-get 'content result) "No relationships found.")))
     t)))

;; ==================== Interactive Commands ====================

(defun lin-agent/memory-search (query)
  "Search MemPalace for QUERY interactively."
  (interactive "sSearch memory: ")
  (let ((result (lin-agent-mcp--call-sync
                 "tools/call"
                 `(("name" . "mempalace_search")
                   ("arguments" . (("query" . ,query) ("limit" . 5)))))))
    (with-current-buffer (get-buffer-create "*Memory Results*")
      (erase-buffer)
      (insert (format "=== Memory Search: %s ===\n\n" query))
      (insert (format "%s" (or (alist-get 'content result) "No results.")))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun lin-agent/memory-store (wing room content)
  "Store CONTENT into MemPalace under WING/ROOM."
  (interactive "sWing: \nsRoom: \nsContent: ")
  (lin-agent-mcp--call-sync
   "tools/call"
   `(("name" . "mempalace_add_drawer")
     ("arguments" . (("wing" . ,wing)
                     ("room" . ,room)
                     ("content" . ,content)
                     ("added_by" . "lin-agent")))))
  (message "Stored in %s/%s." wing room))

;; ==================== Auto-Memory: Recall ====================

(defun lin-agent-memory--auto-recall (prompt)
  "Search memory for context related to PROMPT and return formatted string.
Returns nil if no relevant memories found or MCP not connected."
  (when (and lin-agent-auto-recall
             lin-agent-memory-enabled
             lin-agent-mcp--process)
    (condition-case nil
        (let ((result (lin-agent-mcp--call-sync
                       "tools/call"
                       `(("name" . "mempalace_search")
                         ("arguments" . (("query" . ,prompt) ("limit" . 3))))
                       5)))
          (let ((content (alist-get 'content result)))
            (when (and content (not (string-empty-p (format "%s" content))))
              (format "\n[Auto-recalled memories]\n%s\n[End of recalled memories]\n"
                      content))))
      (error nil))))

;; ==================== Auto-Memory: Session Diary ====================

(defun lin-agent-memory--auto-diary ()
  "Automatically write a session summary to MemPalace diary.
Called when session ends. Extracts key info from conversation."
  (when (and lin-agent-auto-memory
             lin-agent-memory-enabled
             lin-agent-mcp--process
             lin-agent--messages
             (> lin-agent--turn-count 0))
    (condition-case err
        (let* ((summary (lin-agent-memory--build-session-summary))
               (result (lin-agent-mcp--call-sync
                        "tools/call"
                        `(("name" . "mempalace_diary_write")
                          ("arguments" . (("agent_name" . "lin-agent")
                                          ("entry" . ,summary)
                                          ("topic" . "session"))))
                        10)))
          (message "Session diary auto-saved to MemPalace."))
      (error
       (message "Auto-diary failed: %s" (error-message-string err))))))

(defun lin-agent-memory--build-session-summary ()
  "Build a compact session summary from message history."
  (let ((user-msgs nil)
        (tool-names nil))
    (dolist (msg (or lin-agent--messages nil))
      (let ((role (alist-get 'role msg))
            (content (alist-get 'content msg)))
        (when (and (equal role "user") (stringp content) (> (length content) 0))
          (push (truncate-string-to-width content 120 nil nil "...") user-msgs))
        (when (vectorp content)
          (mapc (lambda (block)
                  (when (equal (alist-get 'type block) "tool_use")
                    (let ((name (alist-get 'name block)))
                      (when name (push name tool-names)))))
                content))))
    (let ((topics (nreverse user-msgs)))
      (format "Session %s | %d turns | Model: %s\nTopics: %s\nTools used: %s"
              (format-time-string "%Y-%m-%d %H:%M")
              (or lin-agent--turn-count 0)
              (or lin-agent-model "unknown")
              (if topics
                  (mapconcat #'identity (cl-subseq topics 0 (min 3 (length topics))) " → ")
                "(none)")
              (if tool-names
                  (mapconcat #'identity (delete-dups (nreverse tool-names)) ", ")
                "(none)")))))

(provide 'memory-bridge)
;;; memory-bridge.el ends here
