;;; package.el --- 包管理核心模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024
;; Description: 提供统一的包管理接口，包括包源配置、智能安装和修复功能

;; ==================== 常量定义 ====================
(defconst my/package-archives-cn
  '(("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ("gnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ("nongnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/"))
  "国内镜像源列表")

(defconst my/package-archives-default
  '(("melpa" . "https://melpa.org/packages/")
    ("gnu" . "https://elpa.gnu.org/packages/")
    ("org" . "https://orgmode.org/elpa/"))
  "默认包源列表")

(defconst my/package-priorities
  '(("melpa-cn" . 10)
    ("gnu-cn" . 9)
    ("nongnu-cn" . 8)
    ("melpa" . 7)
    ("gnu" . 6)
    ("org" . 5))
  "包源优先级配置")

;; ==================== 包源配置 ====================
(defun my/setup-package-archives ()
  "配置包源，合并国内镜像和默认源"
  (setq package-archives (append my/package-archives-cn my/package-archives-default)
        package-archive-priorities my/package-priorities
        package-check-signature 'allow-unsigned
        package-enable-at-startup nil)
  
  ;; 确保包管理器已初始化
  (unless package--initialized
    (package-initialize)))

;; ==================== 网络优化 ====================
(defun my/optimize-network-settings ()
  "优化网络设置，提高包下载速度"
  (setq url-retry-attempts 5
        url-connection-timeout 30
        url-keepalive t
        url-automatic-caching t)
  
  ;; 仅在未设置时配置 ELPA 超时
  (unless (boundp 'dotspacemacs-elpa-timeout)
    (setq dotspacemacs-elpa-timeout 60)))

;; ==================== 智能包安装 ====================
(defun my/smart-package-install (package &optional version retry-count)
  "智能包安装，支持版本指定和重试机制
参数:
  PACKAGE - 包名称（symbol）
  VERSION - 可选的版本号（string）
  RETRY-COUNT - 重试次数，默认为3"
  (unless (symbolp package)
    (error "包名必须是 symbol 类型: %s" package))
  
  (let ((retries (or retry-count 3))
        (success nil)
        (package-name (if version 
                          (intern (format "%s-%s" (symbol-name package) version))
                        package)))
    
    (while (and (> retries 0) (not success))
      (condition-case err
          (progn
            (unless (package-installed-p package-name)
              (package-install package-name))
            (setq success t)
            (message "✓ 包 %s 安装成功" package-name))
        (error
         (message "✗ 包 %s 安装失败 (剩余重试: %d): %s" 
                  package-name retries (error-message-string err))
         (setq retries (1- retries))
         (when (> retries 0)
           (sleep-for 2)))))
    
    success))

;; ==================== 包修复功能 ====================
(defun my/fix-missing-packages ()
  "修复所有缺失的包，支持关键包和LSP包"
  (interactive)
  (message "开始修复缺失的包...")
  
  ;; 确保包源已配置
  (my/setup-package-archives)
  
  ;; 刷新包列表（如果需要）
  (unless package-archive-contents
    (message "刷新包列表...")
    (package-refresh-contents))
  
  ;; 修复关键包
  (my/fix-critical-packages)
  
  ;; 修复LSP相关包
  (my/fix-lsp-packages)
  
  (message "✓ 包修复完成！请重启 Emacs 以应用更改"))

(defun my/fix-critical-packages ()
  "修复关键包，包括已知的版本问题包"
  (let ((critical-packages '((spinner . "1.7.2")
                             (auctex . "11.87")
                             (mathjax . "0.1"))))
    (dolist (pkg-spec critical-packages)
      (let ((pkg (car pkg-spec))
            (version (cdr pkg-spec)))
        (unless (package-installed-p pkg)
          (message "安装关键包: %s (版本: %s)" pkg version)
          (my/smart-package-install pkg version))))))

(defun my/fix-lsp-packages ()
  "修复LSP相关包"
  (let ((lsp-packages '(lsp-mode lsp-ui company)))
    (dolist (pkg lsp-packages)
      (unless (package-installed-p pkg)
        (message "安装LSP包: %s" pkg)
        (my/smart-package-install pkg)))))

;; ==================== 包管理工具 ====================
(defun my/force-refresh-packages ()
  "强制刷新包列表，重新配置包源"
  (interactive)
  (message "强制刷新包列表...")
  (my/setup-package-archives)
  (my/optimize-network-settings)
  (condition-case err
      (progn
        (package-refresh-contents)
        (message "✓ 包列表刷新完成"))
    (error
     (message "✗ 包列表刷新失败: %s" (error-message-string err)))))

(defun my/clean-package-cache ()
  "清理包缓存目录"
  (interactive)
  (let ((cache-dir (expand-file-name "elpa" user-emacs-directory)))
    (if (file-exists-p cache-dir)
        (condition-case err
            (progn
              (delete-directory cache-dir t)
              (message "✓ 包缓存已清理: %s" cache-dir))
          (error
           (message "✗ 清理包缓存失败: %s" (error-message-string err))))
      (message "包缓存目录不存在: %s" cache-dir))))

(defun my/reset-package-archives ()
  "重置包源配置"
  (interactive)
  (setq package-archives nil
        package-archive-contents nil)
  (my/setup-package-archives)
  (message "✓ 包源已重置"))

;; ==================== 初始化 ====================
(defun my/init-package ()
  "初始化包管理模块"
  (message "初始化包管理器...")
  
  ;; 配置包源
  (my/setup-package-archives)
  
  ;; 优化网络设置
  (my/optimize-network-settings)
  
  ;; 刷新包列表（如果需要）
  (unless package-archive-contents
    (condition-case err
        (package-refresh-contents)
      (error
       (message "✗ 包列表刷新失败: %s" (error-message-string err)))))
  
  (message "✓ 包管理器初始化完成"))

;; 立即应用包源配置
(my/setup-package-archives)
(my/optimize-network-settings)

;; 延迟初始化（避免与 Spacemacs 初始化冲突）
(run-with-timer 2 nil #'my/init-package)

(provide 'package)
;;; package.el ends here
