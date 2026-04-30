;;; lin-agent-mentions.el --- Cursor-style @file / @folder context -*- lexical-binding: t; -*-
;; Author: xuzhifeng

(require 'cl-lib)
(require 'subr-x)

(defcustom lin-agent-mention-max-file-chars 120000
  "Max characters to read from a single @mention file."
  :type 'integer
  :group 'lin-agent)

(defcustom lin-agent-mention-max-dir-files 120
  "Max file paths to list for a @folder mention."
  :type 'integer
  :group 'lin-agent)

(defun lin-agent--mention-project-root ()
  (ignore-errors
    (when (fboundp 'projectile-project-root)
      (projectile-project-root))))

(defun lin-agent--mention-project-root-from-ctx (ctx)
  (or (plist-get ctx :project-root)
      (let ((default-directory
             (or (plist-get ctx :default-directory) default-directory)))
        (lin-agent--mention-project-root))))

(defun lin-agent--mention-resolve (raw &optional ctx)
  "Resolve RAW path to an absolute existing file or directory, or nil."
  (when (and raw (not (string-empty-p (string-trim raw))))
    (let* ((trimmed (string-trim raw))
           (root (lin-agent--mention-project-root-from-ctx ctx))
           (bf (when ctx (plist-get ctx :buffer-file)))
           (buffer-dir (when bf (file-name-directory bf)))
           (dd (when ctx (plist-get ctx :default-directory)))
           (candidates
            (delq nil
                  (list (expand-file-name trimmed)
                        (when dd (expand-file-name trimmed dd))
                        (when root (expand-file-name trimmed root))
                        (when buffer-dir (expand-file-name trimmed buffer-dir))
                        (expand-file-name trimmed default-directory)))))
      (cl-find-if
       (lambda (p)
         (and p (or (file-exists-p p) (file-directory-p p))))
       (cl-delete-duplicates candidates :test #'equal)))))

(defun lin-agent--mention-strip-trailing-punct (s)
  (let ((out s)
        (punct '(?. ?, ?\; ?: ?! ?? ?\) ?\] ?})))
    (while (and (> (length out) 0)
                (memq (aref out (1- (length out))) punct))
      (setq out (substring out 0 (1- (length out)))))
    out))

(defun lin-agent--mention-file-block (path)
  (cond
   ((not (file-readable-p path))
    (format "(unreadable: %s)\n" path))
   (t
    (with-temp-buffer
      (insert-file-contents-literally path)
      (let* ((text (buffer-string))
             (max-chars lin-agent-mention-max-file-chars))
        (if (> (length text) max-chars)
            (concat (substring text 0 max-chars)
                    (format "\n\n… [truncated at %d chars]\n" max-chars))
          text))))))

(defun lin-agent--mention-dir-block (dir)
  (if (not (file-accessible-directory-p dir))
      (format "(inaccessible directory: %s)\n" dir)
    (let ((files nil)
          (n 0)
          (lim lin-agent-mention-max-dir-files))
      (condition-case nil
          (dolist (f (directory-files-recursively (directory-file-name dir) ".*" nil))
            (when (>= n lim) (cl-return))
            (push f files)
            (cl-incf n))
        (error nil))
      (concat "Recursive paths (capped):\n"
              (mapconcat (lambda (f) (format "  %s" f)) (nreverse files) "\n")
              "\n"))))

(defun lin-agent--mention-parse-next (prompt start)
  (let ((case-fold-search nil))
    (when (string-match
           "\\(?:@\"\\([^\"]+\\)\"\\|@'\\([^']+\\)'\\|@\\([^ \t\n]+\\)\\)"
           prompt start)
      (list (match-beginning 0)
            (match-end 0)
            (or (and (match-string 1 prompt) (string-trim (match-string 1 prompt)))
                (and (match-string 2 prompt) (string-trim (match-string 2 prompt)))
                (lin-agent--mention-strip-trailing-punct (match-string 3 prompt)))))))

(defun lin-agent--mention-format-block (resolved seen)
  "Return attachment string for RESOLVED path, or nil if duplicate."
  (when (and resolved (not (gethash resolved seen)))
    (puthash resolved t seen)
    (concat (format "### %s (%s)\n"
                    resolved
                    (if (file-directory-p resolved) "directory" "file"))
            (if (file-directory-p resolved)
                (lin-agent--mention-dir-block resolved)
              (lin-agent--mention-file-block resolved))
            "\n")))

(defun lin-agent-expand-mentions (prompt &optional ctx)
  "Expand @path mentions; CTX plist: :default-directory :buffer-file :project-root."
  (if (or (null prompt) (not (string-match "@" prompt)))
      prompt
    (let ((pos 0)
          (attachments nil)
          (seen (make-hash-table :test 'equal))
          (body-parts nil))
      (while (< pos (length prompt))
        (let ((next (lin-agent--mention-parse-next prompt pos)))
          (if (null next)
              (progn
                (push (substring prompt pos) body-parts)
                (setq pos (length prompt)))
            (let* ((mb (nth 0 next))
                   (me (nth 1 next))
                   (raw (nth 2 next))
                   (resolved (lin-agent--mention-resolve raw ctx))
                   (block (lin-agent--mention-format-block resolved seen)))
              (when (> mb pos)
                (push (substring prompt pos mb) body-parts))
              (when block
                (setq attachments (append attachments (list block))))
              (unless resolved
                (push (substring prompt mb me) body-parts))
              (setq pos me)))))
      (let ((body (apply #'concat (nreverse body-parts))))
        (if attachments
            (concat "--- Context from @mentions (Cursor-style) ---\n\n"
                    (mapconcat #'identity attachments "\n")
                    "--- End @mentions ---\n\n"
                    body)
          body)))))

(defun lin-agent-insert-mention ()
  "Insert @path with completion (project files under ROOT, bounded)."
  (interactive)
  (let* ((root (or (lin-agent--mention-project-root)
                   (when buffer-file-name (file-name-directory buffer-file-name))
                   default-directory))
         (table (lin-agent--mention-completion-table root))
         (choice (completing-read "@Attach file or folder: " table nil nil)))
    (when (and choice (not (string-empty-p choice)))
      (let* ((full (expand-file-name choice root))
             (rel (if (file-name-absolute-p choice)
                      (if (and root (string-prefix-p (file-truename root) (file-truename full)))
                          (file-relative-name full root)
                        full)
                    choice)))
        (insert (if (string-match-p "[ \t\n]" rel)
                    (format "@\"%s\"" rel)
                  (concat "@" rel)))))))

(defun lin-agent--mention-completion-table (root)
  (let ((out nil) (n 0))
    (when (and root (file-directory-p root))
      (condition-case nil
          (dolist (f (directory-files-recursively (directory-file-name root) ".*" nil))
            (when (>= n 3000) (cl-return))
            (push (file-relative-name f root) out)
            (cl-incf n))
        (error nil)))
    (sort (cl-delete-duplicates out :test #'equal) #'string<)))

(provide 'lin-agent-mentions)

;;; lin-agent-mentions.el ends here
