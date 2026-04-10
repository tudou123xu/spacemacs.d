;;; lin-agent-skills.el --- Skill loader for lin-agent. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Aligns with Claude Code's skills/ system: discover and inject SKILL.md files.

(require 'cl-lib)

;; ==================== Skill Discovery ====================

(defvar lin-agent-skill-paths
  '("~/.claude/skills/"
    "~/.cursor/skills/")
  "Directories to search for skill packages (each containing SKILL.md).")

(defvar lin-agent--loaded-skills nil
  "List of currently loaded skill plists: ((:name :path :content)).")

(defun lin-agent-skill--discover ()
  "Discover all SKILL.md files under `lin-agent-skill-paths'."
  (let (skills)
    (dolist (base lin-agent-skill-paths)
      (let ((expanded (expand-file-name base)))
        (when (file-directory-p expanded)
          (dolist (dir (directory-files expanded t "^[^.]"))
            (when (file-directory-p dir)
              (let ((skill-file (expand-file-name "SKILL.md" dir)))
                (when (file-exists-p skill-file)
                  (push (list :name (file-name-nondirectory dir)
                              :path skill-file
                              :content nil)
                        skills))))))))
    (nreverse skills)))

(defun lin-agent-skill--load (skill)
  "Load SKILL content from disk. Returns skill with :content populated."
  (let ((path (plist-get skill :path)))
    (when (file-exists-p path)
      (plist-put skill :content
                 (with-temp-buffer
                   (insert-file-contents path)
                   (buffer-string)))))
  skill)

;; ==================== Skill Activation ====================

(defun lin-agent-skill-load-all ()
  "Discover and load all available skills."
  (interactive)
  (setq lin-agent--loaded-skills
        (mapcar #'lin-agent-skill--load (lin-agent-skill--discover)))
  (message "Loaded %d skills: %s"
           (length lin-agent--loaded-skills)
           (mapconcat (lambda (s) (plist-get s :name))
                      lin-agent--loaded-skills ", ")))

(defun lin-agent-skill-load-by-name (name)
  "Load a specific skill by NAME and add to active skills."
  (interactive
   (list (completing-read "Skill: "
                          (mapcar (lambda (s) (plist-get s :name))
                                  (lin-agent-skill--discover))
                          nil t)))
  (let* ((all (lin-agent-skill--discover))
         (skill (cl-find name all :key (lambda (s) (plist-get s :name)) :test #'equal)))
    (when skill
      (lin-agent-skill--load skill)
      (push skill lin-agent--loaded-skills)
      (message "Loaded skill: %s" name))))

;; ==================== Skill Injection into System Prompt ====================

(defun lin-agent-skill--prompt-section ()
  "Build the skills section for system prompt injection.
Returns nil if no skills loaded."
  (when lin-agent--loaded-skills
    (let ((sections
           (mapcar (lambda (s)
                     (format "### Skill: %s\n%s"
                             (plist-get s :name)
                             (or (plist-get s :content) "(not loaded)")))
                   lin-agent--loaded-skills)))
      (concat "\n\n## Active Skills\n"
              (mapconcat #'identity sections "\n\n")))))

(defun lin-agent/list-skills ()
  "List all discovered and loaded skills."
  (interactive)
  (let ((discovered (lin-agent-skill--discover)))
    (with-current-buffer (get-buffer-create "*Agent Skills*")
      (erase-buffer)
      (insert "=== Discovered Skills ===\n\n")
      (dolist (s discovered)
        (let ((loaded (cl-find (plist-get s :name) lin-agent--loaded-skills
                               :key (lambda (x) (plist-get x :name)) :test #'equal)))
          (insert (format "  %s %s\n    %s\n\n"
                          (if loaded "[loaded]" "[available]")
                          (plist-get s :name)
                          (plist-get s :path)))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'lin-agent-skills)
;;; skills.el ends here
