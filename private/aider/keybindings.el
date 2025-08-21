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
  
  ;;  "oat"  #'aider/toggle-auto-commit
  
  ;; DeepSeek 集成功能
  "oad"  #'aider/deepseek-code-review        ; DeepSeek 代码审查
  "aao"  #'aider/deepseek-optimize-code      ; DeepSeek 代码优化
  "aag"  #'aider/deepseek-generate-tests     ; DeepSeek 测试生成
  "aad"  #'aider/deepseek-documentation      ; DeepSeek 文档生成
  "aas"  #'aider/switch-provider              ; 切换 AI 提供商
  "aar"  #'aider/smart-task-router            ; 智能任务路由)

;; ==================== 模式特定绑定 ====================
(spacemacs/set-leader-keys-for-major-mode 'prog-mode
  "ar"   #'aider/review-current-function
  "aa"   #'aider/add-current-file
  "ae"   #'aider/explain-region
  
  ;; DeepSeek 编程模式绑定
  "ad"   #'aider/deepseek-code-review        ; 代码审查
  "ao"   #'aider/deepseek-optimize-code      ; 代码优化
  "ag"   #'aider/deepseek-generate-tests     ; 测试生成
  "ad"   #'aider/deepseek-documentation      ; 文档生成)

;; ==================== 全局快捷键 ====================
(global-set-key (kbd "C-c a s") #'aider/start-session)
(global-set-key (kbd "C-c a a") #'aider/add-current-file)
(global-set-key (kbd "C-c a r") #'aider/review-current-function)

;; DeepSeek 全局快捷键
(global-set-key (kbd "C-c a d") #'aider/deepseek-code-review)
(global-set-key (kbd "C-c a o") #'aider/deepseek-optimize-code)
(global-set-key (kbd "C-c a g") #'aider/deepseek-generate-tests)
(global-set-key (kbd "C-c a t") #'aider/deepseek-documentation)
(global-set-key (kbd "C-c a p") #'aider/switch-provider)

;; ==================== 上下文菜单绑定 ====================
(with-eval-after-load 'evil
  (evil-define-key 'visual 'global
    (kbd "SPC o a e") #'aider/explain-region
    (kbd "SPC o a o") #'aider/deepseek-optimize-code))

;; ==================== 快速访问菜单 ====================
(defun aider/quick-menu ()
  "显示 Aider 快速访问菜单"
  (interactive)
  (let ((menu-options '("代码审查" "代码优化" "测试生成" "文档生成" "切换模型" "智能路由")))
    (let ((choice (completing-read "选择 DeepSeek 功能: " menu-options)))
      (pcase choice
        ("代码审查" (aider/deepseek-code-review (buffer-file-name)))
        ("代码优化" (if (region-active-p)
                        (aider/deepseek-optimize-code (region-beginning) (region-end))
                      (message "请先选择要优化的代码区域")))
        ("测试生成" (aider/deepseek-generate-tests (buffer-file-name)))
        ("文档生成" (aider/deepseek-documentation (buffer-file-name)))
        ("切换模型" (aider/switch-provider nil))
        ("智能路由" (aider/smart-task-router nil))))))

;; 添加快捷键到快速菜单
(global-set-key (kbd "C-c a m") #'aider/quick-menu)
(spacemacs/set-leader-keys "aam" #'aider/quick-menu)

(provide 'keybindings)
;;; keybindings.el ends here
