;;; fix-missing-packages.el --- 修复缺失包问题 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 缺失包修复主函数 ====================
(defun my/fix-missing-packages ()
  "修复所有缺失的包"
  (interactive)
  (message "开始修复缺失的包...")
  
  ;; 设置包源
  (my/setup-package-archives)
  
  ;; 刷新包列表
  (my/refresh-package-archives)
  
  ;; 修复关键包
  (my/fix-critical-packages)
  
  ;; 修复LSP相关包
  (my/fix-lsp-packages)
  
  ;; 修复LaTeX相关包
  (my/fix-latex-packages)
  
  (message "✓ 包修复完成！请重启 Emacs 以应用更改"))

;; ==================== 快速修复命令 ====================
(defun my/quick-fix-packages ()
  "快速修复缺失包（简化版）"
  (interactive)
  (message "快速修复缺失包...")
  
  ;; 直接安装关键包
  (let ((packages '(spinner auctex mathjax lsp-mode lsp-ui dap-mode)))
    (dolist (pkg packages)
      (when (not (package-installed-p pkg))
        (message "安装 %s..." pkg)
        (condition-case err
            (progn
              (package-install pkg)
              (message "✓ %s 安装成功" pkg))
          (error
           (message "✗ %s 安装失败: %s" pkg (error-message-string err)))))))
  
  (message "✓ 快速修复完成！"))

;; ==================== 包源配置 ====================
(defun my/setup-package-archives ()
  "配置包源"
  (setq package-archives
        '(("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ("gnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ("nongnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
          ("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")
          ("org" . "https://orgmode.org/elpa/"))
        package-archive-priorities
        '(("melpa-cn" . 10)
          ("gnu-cn" . 9)
          ("nongnu-cn" . 8)
          ("melpa" . 7)
          ("gnu" . 6)
          ("org" . 5))
        package-check-signature 'allow-unsigned)
  
  (package-initialize))

;; ==================== 刷新包列表 ====================
(defun my/refresh-package-archives ()
  "刷新包列表"
  (message "刷新包列表...")
  (condition-case err
      (progn
        (package-refresh-contents)
        (message "✓ 包列表刷新成功"))
    (error
     (message "⚠ 包列表刷新失败: %s" (error-message-string err)))))

;; ==================== 修复关键包 ====================
(defun my/fix-critical-packages ()
  "修复关键依赖包"
  (message "修复关键依赖包...")
  
  (let ((critical-packages
         '((spinner . "1.7.2")
           (auctex . "11.87")
           (mathjax . "0.1"))))
    
    (dolist (pkg-spec critical-packages)
      (let ((pkg (car pkg-spec))
            (version (cdr pkg-spec)))
        (my/install-package-with-fallback pkg version)))))

;; ==================== 修复LSP相关包 ====================
(defun my/fix-lsp-packages ()
  "修复LSP相关包"
  (message "修复LSP相关包...")
  
  (let ((lsp-packages
         '(lsp-mode lsp-ui lsp-treemacs lsp-pyright lsp-origami
           lsp-latex lsp-java lsp-ivy dap-mode)))
    
    (dolist (pkg lsp-packages)
      (my/install-package-with-fallback pkg))))

;; ==================== 修复LaTeX相关包 ====================
(defun my/fix-latex-packages ()
  "修复LaTeX相关包"
  (message "修复LaTeX相关包...")
  
  (let ((latex-packages
         '(evil-tex company-auctex)))
    
    (dolist (pkg latex-packages)
      (my/install-package-with-fallback pkg))))

;; ==================== 智能包安装 ====================
(defun my/install-package-with-fallback (package &optional version)
  "智能安装包，带降级策略"
  (when (not (package-installed-p package))
    (message "安装包: %s" package)
    
    ;; 尝试安装指定版本
    (if version
        (my/try-install-specific-version package version)
      (my/try-install-latest-version package))))

(defun my/try-install-specific-version (package version)
  "尝试安装指定版本"
  (condition-case err
      (progn
        (package-install (intern (concat (symbol-name package) "-" version)))
        (message "✓ %s-%s 安装成功" package version))
    (error
     (message "✗ %s-%s 安装失败，尝试最新版本" package version)
     (my/try-install-latest-version package))))

(defun my/try-install-latest-version (package)
  "尝试安装最新版本"
  (condition-case err
      (progn
        (package-install package)
        (message "✓ %s 最新版本安装成功" package))
    (error
     (message "✗ %s 安装失败: %s" package (error-message-string err))
     (my/try-alternative-sources package))))

(defun my/try-alternative-sources (package)
  "尝试从其他源安装"
  (message "尝试从其他源安装 %s..." package)
  
  ;; 这里可以添加从其他源安装的逻辑
  (message "⚠ 无法从任何源安装 %s" package))

;; ==================== 包状态检查 ====================
(defun my/check-package-status ()
  "检查包安装状态"
  (interactive)
  (message "检查包安装状态...")
  
  (let ((required-packages
         '(spinner auctex mathjax lsp-mode lsp-ui dap-mode
           evil-tex company-auctex devdocs))
        (missing-packages '())
        (installed-packages '()))
    
    (dolist (pkg required-packages)
      (if (package-installed-p pkg)
          (push pkg installed-packages)
        (push pkg missing-packages)))
    
    (message "已安装包 (%d): %s" 
             (length installed-packages) 
             (string-join (mapcar 'symbol-name installed-packages) ", "))
    
    (when missing-packages
      (message "缺失包 (%d): %s" 
               (length missing-packages) 
               (string-join (mapcar 'symbol-name missing-packages) ", "))
      (message "运行 M-x my/fix-missing-packages 来修复缺失的包"))))

;; ==================== 提供模块 ====================
(provide 'fix-missing-packages)
;;; fix-missing-packages.el ends here
