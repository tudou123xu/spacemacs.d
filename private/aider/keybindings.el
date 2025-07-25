;;; keybindings.el --- Aider 快捷键绑定 -*- lexical-binding: t; -*-

;; ==================== Spacemacs Leader Key 绑定 ====================
(spacemacs/set-leader-keys
  ;; Aider 会话管理
  "oas"  #'aider/start-session
  "oaq"  #'aider/quit-session
  "oaa"  #'aider/add-current-file
  "oah"  #'aider/show-help
  "oal"  #'aider/list-files
  
  ;; 代码审查和解释
  "oar"  #'aider/review-current-function
  "oae"  #'aider/explain-region
  
  ;; 设置
  "oat"  #'aider/toggle-auto-commit)

;; ==================== 模式特定绑定 ====================
(spacemacs/set-leader-keys-for-major-mode 'prog-mode
  "ar"   #'aider/review-current-function
  "aa"   #'aider/add-current-file
  "ae"   #'aider/explain-region)

;; ==================== 全局快捷键 ====================
(global-set-key (kbd "C-c a s") #'aider/start-session)
(global-set-key (kbd "C-c a a") #'aider/add-current-file)
(global-set-key (kbd "C-c a r") #'aider/review-current-function)

;; ==================== 上下文菜单绑定 ====================
(with-eval-after-load 'evil
  (evil-define-key 'visual 'global
    (kbd "SPC o a e") #'aider/explain-region))

(provide 'keybindings)
;;; keybindings.el ends here
