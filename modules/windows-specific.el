;;; windows-specific.el --- Windows 专属配置 -*- lexical-binding: t; -*-

;; ==================== Windows 系统集成 ====================
(when (spacemacs/system-is-mswindows)
  
  ;; ==================== 文件系统优化 ====================
  ;; Windows 路径处理
  (setq w32-get-true-file-attributes nil  ; 提升性能
        w32-pipe-read-delay 0             ; 减少管道延迟
        w32-pipe-buffer-size (* 64 1024)) ; 增加缓冲区
  
  ;; 禁用 Windows 特有的文件锁定
  (setq create-lockfiles nil)
  
  ;; ==================== 编码设置 ====================
  ;; Windows 默认编码处理
  (set-selection-coding-system 'utf-16le-dos)
  (set-clipboard-coding-system 'utf-16le-dos)
  
  ;; ==================== 字体配置 ====================
  ;; Windows 优化字体渲染
  (setq inhibit-compacting-font-caches t)
  
  ;; ==================== 网络配置 ====================
  ;; Windows 代理设置
  (when (getenv "HTTP_PROXY")
    (setq url-proxy-services
          `(("http" . ,(getenv "HTTP_PROXY"))
            ("https" . ,(or (getenv "HTTPS_PROXY") (getenv "HTTP_PROXY"))))))
  
  ;; ==================== TRAMP 优化 ====================
  ;; Windows 下的 TRAMP 配置
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPersist=60 -o ConnectTimeout=15")
  
  ;; 使用 plink 作为默认 SSH 客户端（如果可用）
  (when (executable-find "plink")
    (setq tramp-default-method "plink"))
  
  ;; ==================== 外部工具集成 ====================
  ;; Windows Terminal 集成
  (defun windows/open-in-terminal ()
    "在 Windows Terminal 中打开当前目录"
    (interactive)
    (let ((default-directory (or (projectile-project-root) default-directory)))
      (if (executable-find "wt")
          (call-process "wt" nil nil nil "-d" default-directory)
        (call-process "cmd" nil nil nil "/c" "start" "cmd" "/k" 
                      (format "cd /d \"%s\"" default-directory)))))
  
  ;; PowerShell 集成
  (defun windows/open-powershell ()
    "打开 PowerShell"
    (interactive)
    (let ((default-directory (or (projectile-project-root) default-directory)))
      (if (executable-find "pwsh")
          (call-process "pwsh" nil nil nil "-WorkingDirectory" default-directory)
        (call-process "powershell" nil nil nil "-Command" 
                      (format "Set-Location '%s'" default-directory)))))
  
  ;; ==================== 文件关联 ====================
  (defun windows/open-with-default-app ()
    "使用默认应用程序打开文件"
    (interactive)
    (when buffer-file-name
      (call-process "cmd" nil nil nil "/c" "start" "" buffer-file-name)))
  
  ;; ==================== 性能优化 ====================
  ;; Windows 特有的性能设置
  (setq w32-get-true-file-attributes nil
        inhibit-compacting-font-caches t)
  
  ;; 减少不必要的系统调用
  (setq vc-handled-backends '(Git)
        auto-save-default nil)
  
  ;; ==================== 快捷键绑定 ====================
  (global-set-key (kbd "C-c w t") #'windows/open-in-terminal)
  (global-set-key (kbd "C-c w p") #'windows/open-powershell)
  (global-set-key (kbd "C-c w o") #'windows/open-with-default-app)
  
  ;; Windows 风格的快捷键
  (global-set-key (kbd "C-z") #'undo)
  (global-set-key (kbd "C-y") #'redo)
  
  (message "Windows 专属配置已加载"))

(provide 'windows-specific)
;;; windows-specific.el ends here 