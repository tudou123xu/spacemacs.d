;;; mcp-client.el --- General MCP client for lin-agent. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Supports connecting to multiple MCP servers (stdio transport).

(require 'json)
(require 'cl-lib)
(require 'subr-x)

;; ==================== Data Structures ====================

(defvar lin-agent-mcp--servers (make-hash-table :test 'equal)
  "Hash of active MCP servers. Key: name, Value: plist (:process :request-id :callbacks :tools).")

(defvar lin-agent-mcp-server-configs nil
  "List of MCP server configs. Each is (name . plist) where plist has :command :args :env.
Example: ((\"filesystem\" :command \"npx\" :args (\"-y\" \"@modelcontextprotocol/server-filesystem\" \"/tmp\")))")

;; ==================== Connection Management ====================

(defun lin-agent-mcp-connect (name)
  "Connect to MCP server NAME (from `lin-agent-mcp-server-configs')."
  (interactive
   (list (completing-read "MCP server: "
                          (mapcar #'car lin-agent-mcp-server-configs) nil t)))
  (let ((config (alist-get name lin-agent-mcp-server-configs nil nil #'equal)))
    (unless config
      (user-error "Unknown MCP server: %s" name))
    (when (lin-agent-mcp-connected-p name)
      (user-error "Server %s already connected" name))
    (let* ((cmd (plist-get config :command))
           (args (plist-get config :args))
           (proc-name (format "mcp-%s" name))
           (proc (apply #'start-process proc-name nil cmd args)))
      (set-process-filter proc (lin-agent-mcp--make-filter name))
      (set-process-sentinel proc (lin-agent-mcp--make-sentinel name))
      (set-process-coding-system proc 'utf-8 'utf-8)
      (puthash name
               (list :process proc
                     :request-id 0
                     :callbacks (make-hash-table)
                     :tools nil
                     :resources nil)
               lin-agent-mcp--servers)
      ;; Initialize MCP
      (lin-agent-mcp--send-rpc name "initialize"
        `(("protocolVersion" . "2024-11-05")
          ("capabilities" . (("roots" . (("listChanged" . t)))))
          ("clientInfo" . (("name" . "lin-agent") ("version" . "1.0"))))
        (lambda (result)
          (message "MCP server %s initialized: %s"
                   name (alist-get 'serverInfo result))
          ;; Discover tools
          (lin-agent-mcp--send-rpc name "tools/list" nil
            (lambda (result)
              (let ((tools (alist-get 'tools result)))
                (lin-agent-mcp--register-server-tools name tools)
                (message "MCP %s: %d tools discovered" name (length tools))))))))))

(defun lin-agent-mcp-disconnect (name)
  "Disconnect from MCP server NAME."
  (interactive
   (list (completing-read "Disconnect MCP: "
                          (lin-agent-mcp-active-servers) nil t)))
  (when-let ((server (gethash name lin-agent-mcp--servers)))
    (let ((proc (plist-get server :process)))
      (when (process-live-p proc)
        (delete-process proc)))
    (lin-agent-mcp--unregister-server-tools name)
    (remhash name lin-agent-mcp--servers)
    (message "MCP %s disconnected." name)))

(defun lin-agent-mcp-connected-p (name)
  "Return non-nil if MCP server NAME is connected."
  (when-let ((server (gethash name lin-agent-mcp--servers)))
    (process-live-p (plist-get server :process))))

(defun lin-agent-mcp-active-servers ()
  "Return list of active MCP server names."
  (let (names)
    (maphash (lambda (name server)
               (when (process-live-p (plist-get server :process))
                 (push name names)))
             lin-agent-mcp--servers)
    (nreverse names)))

;; ==================== JSON-RPC ====================

(defun lin-agent-mcp--send-rpc (name method params callback)
  "Send JSON-RPC to server NAME."
  (let ((server (gethash name lin-agent-mcp--servers)))
    (unless server (user-error "MCP %s not connected" name))
    (let* ((id (cl-incf (plist-get server :request-id)))
           (request `(("jsonrpc" . "2.0")
                      ("id" . ,id)
                      ("method" . ,method)
                      ,@(when params `(("params" . ,params)))))
           (json-str (concat (json-encode request) "\n")))
      (plist-put server :request-id id)
      (puthash id callback (plist-get server :callbacks))
      (process-send-string (plist-get server :process) json-str))))

(defun lin-agent-mcp-call-tool (name tool-name arguments)
  "Call TOOL-NAME on MCP server NAME with ARGUMENTS. Returns result synchronously."
  (let ((result nil) (done nil) (err nil))
    (lin-agent-mcp--send-rpc name "tools/call"
      `(("name" . ,tool-name) ("arguments" . ,arguments))
      (lambda (r) (setq result r done t)))
    (let ((deadline (+ (float-time) 30)))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.1)))
    (if done
        (let ((content (alist-get 'content result)))
          (if (vectorp content)
              (mapconcat (lambda (c) (or (alist-get 'text c) "")) content "")
            (format "%s" result)))
      (user-error "MCP %s: tool call timeout for %s" name tool-name))))

;; ==================== Process Filters ====================

(defun lin-agent-mcp--make-filter (name)
  "Create a process filter for server NAME."
  (lambda (_process output)
    (dolist (line (split-string output "\n" t))
      (condition-case nil
          (let* ((json-object-type 'alist)
                 (json-key-type 'symbol)
                 (response (json-read-from-string line))
                 (id (alist-get 'id response))
                 (result (alist-get 'result response))
                 (error-obj (alist-get 'error response))
                 (server (gethash name lin-agent-mcp--servers)))
            (when (and server id)
              (let ((callback (gethash id (plist-get server :callbacks))))
                (when callback
                  (remhash id (plist-get server :callbacks))
                  (if error-obj
                      (message "MCP %s error: %s" name (alist-get 'message error-obj))
                    (funcall callback result))))))
        (error nil)))))

(defun lin-agent-mcp--make-sentinel (name)
  "Create a process sentinel for server NAME."
  (lambda (_process event)
    (message "MCP %s: %s" name (string-trim event))))

;; ==================== Tool Registration ====================

(defun lin-agent-mcp--register-server-tools (name tools)
  "Register MCP TOOLS from server NAME into the agent tool registry."
  (when (fboundp 'lin-agent-tool-register)
    (dolist (tool (append tools nil))
      (let* ((tool-name (alist-get 'name tool))
             (description (or (alist-get 'description tool) ""))
             (schema (or (alist-get 'inputSchema tool)
                         '(("type" . "object") ("properties" . ()))))
             (full-name (format "mcp_%s_%s" name tool-name)))
        (lin-agent-tool-register full-name
          (format "[MCP:%s] %s" name description)
          schema
          (lambda (input)
            (lin-agent-mcp-call-tool name tool-name input)))
        ;; Track for cleanup
        (let ((server (gethash name lin-agent-mcp--servers)))
          (when server
            (plist-put server :tools
                       (cons full-name (plist-get server :tools)))))))))

(defun lin-agent-mcp--unregister-server-tools (name)
  "Remove all tools from server NAME."
  (when-let ((server (gethash name lin-agent-mcp--servers)))
    (dolist (tool-name (plist-get server :tools))
      (when (boundp 'lin-agent--tools)
        (remhash tool-name lin-agent--tools)))))

;; ==================== Resource Listing ====================

(defun lin-agent-mcp-list-resources (name)
  "List resources from MCP server NAME."
  (let ((result nil) (done nil))
    (lin-agent-mcp--send-rpc name "resources/list" nil
      (lambda (r) (setq result r done t)))
    (let ((deadline (+ (float-time) 10)))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.1)))
    (when done
      (alist-get 'resources result))))

(provide 'mcp-client)
;;; mcp-client.el ends here
