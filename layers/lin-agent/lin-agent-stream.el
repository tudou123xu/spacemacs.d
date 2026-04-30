;;; lin-agent-stream.el --- SSE streaming for LLM APIs. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Streams LLM API responses token-by-token via curl subprocess.
;; Supports both Claude SSE and OpenAI-compatible SSE formats.

(require 'json)
(require 'cl-lib)
(require 'subr-x)

;; ==================== State ====================

(defvar lin-agent--stream-process nil
  "Active curl process for streaming.")

(defvar lin-agent--stream-buffer ""
  "Partial SSE data buffer for incomplete lines.")

(defvar lin-agent--stream-content-blocks nil
  "Accumulated content blocks from the current streaming response.")

(defvar lin-agent--stream-usage nil
  "Usage data from the streaming response.")

(defvar lin-agent--spinner-timer nil
  "Timer for the animated spinner.")

(defvar lin-agent--spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Animation frames for the thinking spinner.")

(defvar lin-agent--spinner-index 0
  "Current frame index for the spinner.")

(defvar lin-agent--stream-timeout-timer nil
  "Timer for stream timeout protection.")

(defvar lin-agent--stream-last-activity nil
  "Timestamp of last stream activity for timeout detection.")

(defvar lin-agent-stream-timeout 120
  "Seconds of inactivity before a stream is considered stalled.")

(defvar lin-agent--stream-openai-text ""
  "Accumulated text from OpenAI streaming chunks.")

(defvar lin-agent--stream-openai-tool-calls nil
  "Accumulated tool calls from OpenAI streaming.")

(defvar lin-agent--stream-openai-finish nil
  "Finish reason from OpenAI streaming.")

(defvar lin-agent--stream-error-output ""
  "Accumulated non-SSE output (potential error messages) from curl.")

;; ==================== Spinner ====================

(defun lin-agent--spinner-start ()
  "Start the animated thinking spinner in mode line."
  (setq lin-agent--spinner-index 0)
  (lin-agent--spinner-update)
  (setq lin-agent--spinner-timer
        (run-at-time 0.1 0.1 #'lin-agent--spinner-update)))

(defun lin-agent--spinner-stop ()
  "Stop the animated spinner."
  (when lin-agent--spinner-timer
    (cancel-timer lin-agent--spinner-timer)
    (setq lin-agent--spinner-timer nil))
  (when (get-buffer lin-agent--buffer-name)
    (with-current-buffer lin-agent--buffer-name
      (setq mode-line-process nil)
      (force-mode-line-update))))

(defun lin-agent--spinner-update ()
  "Update spinner frame in mode line."
  (when (get-buffer lin-agent--buffer-name)
    (with-current-buffer lin-agent--buffer-name
      (setq mode-line-process
            (format " %s Thinking..."
                    (nth (mod lin-agent--spinner-index
                              (length lin-agent--spinner-frames))
                         lin-agent--spinner-frames)))
      (force-mode-line-update))
    (setq lin-agent--spinner-index (1+ lin-agent--spinner-index))))

;; ==================== SSE Streaming ====================

(defun lin-agent--stream-request (messages tools on-complete)
  "Send streaming request to the current LLM provider.
ON-COMPLETE is called with (response) when the stream ends.
Text is displayed incrementally in the agent buffer."
  (when lin-agent--stream-process
    (delete-process lin-agent--stream-process)
    (setq lin-agent--stream-process nil))

  (setq lin-agent--stream-buffer ""
        lin-agent--stream-content-blocks nil
        lin-agent--stream-usage nil
        lin-agent--stream-openai-text ""
        lin-agent--stream-openai-tool-calls nil
        lin-agent--stream-openai-finish nil
        lin-agent--stream-error-output "")

  (let* ((api-key (lin-ai/get-key lin-agent-provider))
         (api-type (lin-agent--current-api-type))
         (body (lin-agent--build-request messages tools))
         (body-with-stream (cons '("stream" . t) body))
         (json-str (condition-case err
                       (json-encode body-with-stream)
                     (error
                      (lin-agent--display "\n[JSON encoding error: %s]\n"
                                          (error-message-string err))
                      nil)))
         (tmpfile (when json-str (make-temp-file "llm-stream-" nil ".json"))))

    (unless api-key
      (lin-agent--display "\n[API key missing for %s. Use /key to set it]\n" lin-agent-provider)
      (lin-agent--spinner-stop)
      (lin-agent--set-busy nil)
      (user-error "API key not found for %s" lin-agent-provider))

    (unless json-str
      (lin-agent--set-busy nil)
      (user-error "Failed to encode request body"))

    (with-temp-file tmpfile
      (insert json-str))

    (lin-agent--set-busy t)
    (lin-agent--spinner-start)
    (setq lin-agent--stream-last-activity (float-time))
    (when lin-agent--stream-timeout-timer
      (cancel-timer lin-agent--stream-timeout-timer))
    (setq lin-agent--stream-timeout-timer
          (run-at-time lin-agent-stream-timeout nil
                       (lambda ()
                         (when (and lin-agent--stream-process
                                    (> (- (float-time) lin-agent--stream-last-activity)
                                       lin-agent-stream-timeout))
                           (lin-agent--display "\n[Stream timed out after %ds]\n"
                                               lin-agent-stream-timeout)
                           (lin-agent--stream-stop)))))

    (let* ((endpoint (lin-agent--current-endpoint))
           (auth-headers (pcase api-type
                           ('claude (list "-H" (format "x-api-key: %s" api-key)
                                         "-H" (format "anthropic-version: %s" lin-agent-api-version)))
                           ('openai (list "-H" (format "Authorization: Bearer %s" api-key)))))
           (curl-args (append (list "curl" "-sS" "-N" "-X" "POST"
                                    "-H" "Content-Type: application/json")
                              auth-headers
                              (list "-d" (format "@%s" tmpfile) endpoint)))
           (proc (apply #'start-process "llm-stream" nil curl-args)))
      (setq lin-agent--stream-process proc)
      (set-process-coding-system proc 'utf-8 'utf-8)
      (set-process-filter proc
        (lambda (_proc output)
          (condition-case err
              (lin-agent--stream-handle-output output)
            (error
             (message "[lin-agent stream filter error] %s" (error-message-string err))))))
      (set-process-sentinel proc
        (lambda (_proc event)
          (lin-agent--spinner-stop)
          (lin-agent--set-busy nil)
          (ignore-errors (delete-file tmpfile))
          (setq lin-agent--stream-process nil)
          (cond
           ((string-match-p "finished" event)
            (let ((response (lin-agent--stream-build-response)))
              (when (fboundp 'lin-agent--track-cost)
                (lin-agent--track-cost response))
              (funcall on-complete response)))
           ((string-match-p "\\(exited\\|signal\\|failed\\)" event)
            (let ((err-text (string-trim lin-agent--stream-error-output)))
              (if (not (string-empty-p err-text))
                  (condition-case parse-err
                      (let* ((json-object-type 'alist)
                             (json-key-type 'symbol)
                             (err-json (json-read-from-string err-text))
                             (err-msg (or (alist-get 'message (alist-get 'error err-json))
                                          err-text)))
                        (lin-agent--display "\n[API Error: %s]\n" err-msg))
                    (error
                     (lin-agent--display "\n[Stream error: %s]\n" err-text)))
                (lin-agent--display "\n[Stream process %s]\n"
                                    (string-trim event)))))))))))

(defun lin-agent--stream-handle-output (output)
  "Parse SSE OUTPUT and display text incrementally.
Supports both Claude SSE and OpenAI SSE formats."
  (setq lin-agent--stream-last-activity (float-time))
  (setq lin-agent--stream-buffer (concat lin-agent--stream-buffer output))
  (let ((lines (split-string lin-agent--stream-buffer "\n"))
        (api-type (lin-agent--current-api-type)))
    (setq lin-agent--stream-buffer (car (last lines)))
    (dolist (line (butlast lines))
      (let ((trimmed (string-trim-right line "\r")))
        (cond
         ((string-prefix-p "data: " trimmed)
          (let ((data (substring trimmed 6)))
            (unless (equal data "[DONE]")
              (condition-case err
                  (let* ((json-object-type 'alist)
                         (json-key-type 'symbol)
                         (json-array-type 'list)
                         (event (json-read-from-string data)))
                    (pcase api-type
                      ('claude
                       (lin-agent--stream-handle-event (alist-get 'type event) event))
                      ('openai
                       (lin-agent--stream-handle-openai-chunk event))
                      (_
                       (message "[lin-agent] Unknown api-type in stream: %s" api-type))))
                (error
                 (message "[lin-agent SSE parse error] %s | data: %.80s"
                          (error-message-string err) data))))))
         ((and (not (string-empty-p trimmed))
               (not (string-prefix-p ":" trimmed)))
          (setq lin-agent--stream-error-output
                (concat lin-agent--stream-error-output trimmed "\n"))))))))

(defun lin-agent--stream-handle-event (type event)
  "Handle a single Claude SSE EVENT of TYPE."
  (pcase type
    ("content_block_start"
     (let ((block (alist-get 'content_block event)))
       (push (copy-alist block) lin-agent--stream-content-blocks)))

    ("content_block_delta"
     (let* ((delta (alist-get 'delta event))
            (delta-type (alist-get 'type delta)))
       (when (equal delta-type "text_delta")
         (let ((text (alist-get 'text delta)))
           (when text
             (lin-agent--display "%s" text)
             (when lin-agent--stream-content-blocks
               (let ((current (car lin-agent--stream-content-blocks)))
                 (let ((existing (or (alist-get 'text current) "")))
                   (setf (alist-get 'text current) (concat existing text))))))))))

    ("message_delta"
     (let ((usage (alist-get 'usage event)))
       (when usage
         (setq lin-agent--stream-usage
               (append lin-agent--stream-usage
                       `((output_tokens . ,(alist-get 'output_tokens usage)))))))
     (let ((stop-reason (alist-get 'stop_reason (alist-get 'delta event))))
       (when stop-reason
         (setq lin-agent--stream-usage
               (append lin-agent--stream-usage
                       `((stop_reason . ,stop-reason)))))))

    ("message_start"
     (let* ((message (alist-get 'message event))
            (usage (alist-get 'usage message)))
       (when usage
         (setq lin-agent--stream-usage
               `((input_tokens . ,(alist-get 'input_tokens usage))
                 (model . ,(alist-get 'model message)))))))

    ("error"
     (let ((err (alist-get 'error event)))
       (lin-agent--display "\n[Stream Error: %s]\n"
                           (or (alist-get 'message err) (format "%S" err)))))))

(defun lin-agent--stream-handle-openai-chunk (chunk)
  "Handle a single OpenAI SSE CHUNK."
  (let* ((choices (alist-get 'choices chunk))
         (choice (and choices (car choices)))
         (delta (alist-get 'delta choice))
         (finish (alist-get 'finish_reason choice)))
    ;; Handle text content
    (when delta
      (let ((text (alist-get 'content delta)))
        (when (and text (stringp text) (not (string-empty-p text)))
          (lin-agent--display "%s" text)
          (setq lin-agent--stream-openai-text
                (concat lin-agent--stream-openai-text text)))))
    ;; Handle tool calls
    (when-let ((tool-calls (and delta (alist-get 'tool_calls delta))))
      (dolist (tc (if (listp tool-calls) tool-calls (append tool-calls nil)))
        (let ((idx (alist-get 'index tc))
              (id (alist-get 'id tc))
              (fn (alist-get 'function tc)))
          (if id
              (push `((index . ,idx) (id . ,id)
                      (name . ,(alist-get 'name fn))
                      (arguments . ,(or (alist-get 'arguments fn) "")))
                    lin-agent--stream-openai-tool-calls)
            (when-let ((existing (cl-find idx lin-agent--stream-openai-tool-calls
                                          :key (lambda (tc) (alist-get 'index tc)))))
              (setf (alist-get 'arguments existing)
                    (concat (alist-get 'arguments existing)
                            (or (alist-get 'arguments fn) ""))))))))
    ;; Handle finish reason (skip null values)
    (when (and finish (stringp finish))
      (setq lin-agent--stream-openai-finish finish))
    ;; Handle usage
    (when-let ((usage (alist-get 'usage chunk)))
      (setq lin-agent--stream-usage usage))))

(defun lin-agent--stream-build-response ()
  "Build a complete response alist from accumulated stream data."
  (if (eq (lin-agent--current-api-type) 'openai)
      (let* ((content-text lin-agent--stream-openai-text)
             (tool-calls lin-agent--stream-openai-tool-calls)
             (finish (or lin-agent--stream-openai-finish "stop"))
             (message `((role . "assistant")
                        ,@(when (not (string-empty-p content-text))
                            `((content . ,content-text)))
                        ,@(when tool-calls
                            `((tool_calls . ,(vconcat
                                              (mapcar (lambda (tc)
                                                        `((id . ,(alist-get 'id tc))
                                                          (type . "function")
                                                          (function . ((name . ,(alist-get 'name tc))
                                                                       (arguments . ,(alist-get 'arguments tc))))))
                                                      (nreverse tool-calls))))))))
             (oai-response `((choices . (((message . ,message)
                                          (finish_reason . ,finish))))
                             ,@(when lin-agent--stream-usage
                                 `((usage . ,lin-agent--stream-usage))))))
        (lin-agent--normalize-response oai-response))
    ;; Claude format
    (let ((content (nreverse lin-agent--stream-content-blocks))
          (stop-reason (alist-get 'stop_reason lin-agent--stream-usage)))
      `((content . ,content)
        (stop_reason . ,(or stop-reason "end_turn"))
        (usage . ((input_tokens . ,(or (alist-get 'input_tokens lin-agent--stream-usage) 0))
                  (output_tokens . ,(or (alist-get 'output_tokens lin-agent--stream-usage) 0))))))))

;; ==================== Streaming Loop Integration ====================

(defun lin-agent--loop-streaming (messages tools)
  "Streaming version of the agent loop."
  (if lin-agent--busy
      (message "Agent is busy, please wait...")
    (lin-agent--display "\n─── Turn %d [%s] ───\n"
                        (cl-incf lin-agent--turn-count)
                        (or (bound-and-true-p lin-agent--mode) 'agent))

    ;; Micro-compact
    (when (and (fboundp 'lin-agent--should-micro-compact-p)
               (lin-agent--should-micro-compact-p messages))
      (setq messages (lin-agent--micro-compact messages)))

    ;; Full auto-compact
    (when (and (fboundp 'lin-agent--should-compact-p)
               (lin-agent--should-compact-p messages))
      (lin-agent--display "  [Auto-compacting context...]\n")
      (setq messages (lin-agent--compact messages)))

    (lin-agent--stream-request messages tools
      (lambda (response)
        (lin-agent--stream-process-response response messages tools)))))

(defun lin-agent--stream-process-response (response messages tools)
  "Process streaming RESPONSE. Text was already displayed during streaming."
  (condition-case err
      (let ((stop-reason (alist-get 'stop_reason response))
            (content (alist-get 'content response))
            (err-msg (alist-get 'error response)))

        (when err-msg
          (lin-agent--display "\n[API Error: %s]\n"
                              (or (alist-get 'message err-msg) (format "%S" err-msg)))
          (cl-return-from lin-agent--stream-process-response nil))

        (lin-agent--display "\n")
        (lin-agent--scroll-to-bottom)

        (pcase stop-reason
          ("tool_use"
           (let* ((tool-uses (cl-remove-if-not
                               (lambda (block)
                                 (equal (alist-get 'type block) "tool_use"))
                               content))
                  (filtered-uses
                   (if (memq (bound-and-true-p lin-agent--mode) '(plan ask))
                       (cl-remove-if-not
                        (lambda (tu) (lin-agent--tool-readonly-p (alist-get 'name tu)))
                        tool-uses)
                     tool-uses))
                  (blocked (cl-set-difference tool-uses filtered-uses))
                  (assistant-msg `((role . "assistant")
                                   (content . ,(vconcat content))))
                  (results
                   (append
                    (lin-agent--execute-tools-smart filtered-uses)
                    (mapcar (lambda (tu)
                              (lin-agent--make-tool-result
                               (alist-get 'id tu)
                               (format "Tool '%s' blocked in %s mode."
                                       (alist-get 'name tu) lin-agent--mode)
                               t))
                            blocked)))
                  (user-msg `((role . "user")
                              (content . ,(vconcat results)))))
             (lin-agent--loop-streaming
              (append messages (list assistant-msg user-msg))
              tools)))

          ("end_turn"
           (let ((assistant-msg `((role . "assistant")
                                  (content . ,(vconcat content)))))
             (setq lin-agent--messages (append messages (list assistant-msg))))
           (lin-agent--display "─── Session: %d turns" lin-agent--turn-count)
           (when (bound-and-true-p lin-agent--cost-data)
             (lin-agent--display " | $%.4f"
                                 (plist-get lin-agent--cost-data :total-cost)))
           (lin-agent--display " ───\n")
           (when (get-buffer lin-agent--buffer-name)
             (with-current-buffer lin-agent--buffer-name
               (when (fboundp 'lin-agent--insert-prompt)
                 (lin-agent--insert-prompt))))
           (lin-agent--scroll-to-bottom))

          ("max_tokens"
           (lin-agent--display "[Reached max tokens, continuing...]\n")
           (let ((assistant-msg `((role . "assistant")
                                  (content . ,(vconcat content)))))
             (lin-agent--loop-streaming
              (append messages
                      (list assistant-msg
                            (lin-agent--make-user-msg "Continue.")))
              tools)))

          (_
           (lin-agent--display "[Unexpected stop_reason: %s]\n" stop-reason))))
    (error
     (lin-agent--display "\n[Response Error: %s]\n" (error-message-string err)))))

;; ==================== Stop Streaming ====================

(defun lin-agent--stream-stop ()
  "Stop the active stream."
  (interactive)
  (when lin-agent--stream-timeout-timer
    (cancel-timer lin-agent--stream-timeout-timer)
    (setq lin-agent--stream-timeout-timer nil))
  (when lin-agent--stream-process
    (delete-process lin-agent--stream-process)
    (setq lin-agent--stream-process nil))
  (lin-agent--spinner-stop)
  (lin-agent--set-busy nil)
  (lin-agent--display "\n[Stream interrupted]\n"))

(provide 'lin-agent-stream)
;;; lin-agent-stream.el ends here
