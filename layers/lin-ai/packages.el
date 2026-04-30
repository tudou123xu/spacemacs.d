;;; packages.el --- lin-ai layer packages for Spacemacs. -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Provides: gptel (core LLM client), ellama, copilot

(defconst lin-ai-packages
  '(gptel
    ellama
    (copilot :location (recipe :fetcher github
                                :repo "copilot-emacs/copilot.el"
                                :files ("*.el"))))
  "Packages required by lin-ai layer.")

;; ==================== gptel: Core LLM Engine ====================

(defun lin-ai/init-gptel ()
  (use-package gptel
    :defer t
    :commands (gptel gptel-send gptel-rewrite gptel-menu gptel-abort)
    :init
    (setq gptel-default-mode 'org-mode)
    :config
    (lin-ai/setup-gptel-backends)
    (setq gptel-directives
          '((default    . "You are a senior software engineer. Be concise and precise. Respond in the same language as the user.")
            (code       . "You are an expert programmer. Write clean, idiomatic code with no unnecessary comments. Only output code unless asked for explanation.")
            (review     . "You are a code reviewer. Point out bugs, performance issues, and suggest improvements. Be specific.")
            (refactor   . "You are a refactoring expert. Improve code structure, readability, and performance without changing behavior.")
            (explain    . "You are a technical educator. Explain the code clearly with context. Use the same language as the user.")
            (fix        . "You are a debugging expert. Analyze the error, find the root cause, and provide the minimal fix. Show only the corrected code.")
            (docstring  . "You are a documentation writer. Generate clear, complete docstrings/comments for the given code. Follow the language's doc conventions.")
            (architect  . "You are a system architect. Design clean APIs, suggest patterns, evaluate trade-offs. Think about scalability and maintainability.")))))

;; ==================== ellama ====================

(defun lin-ai/init-ellama ()
  (use-package ellama
    :defer t
    :init
    (setq ellama-auto-scroll t)))

;; ==================== copilot (optional) ====================

(defun lin-ai/init-copilot ()
  (use-package copilot
    :defer t
    :hook ((prog-mode . copilot-mode)
           (yaml-mode . copilot-mode)
           (conf-mode . copilot-mode))
    :bind (:map copilot-completion-map
                ("TAB"   . copilot-accept-completion)
                ("<tab>" . copilot-accept-completion)
                ("M-f"   . copilot-accept-completion-by-word)
                ("M-n"   . copilot-next-completion)
                ("M-p"   . copilot-previous-completion)
                ("C-g"   . copilot-clear-overlay))
    :config
    (setq copilot-indent-offset-warning-disable t)))

;;; packages.el ends here
