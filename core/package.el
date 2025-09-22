;;; package.el --- 包管理核心模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 包管理配置常量 ====================
(defconst my/package-config
  '(:archives (("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
               ("gnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
               ("nongnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
               ("melpa" . "https://melpa.org/packages/")
               ("gnu" . "https://elpa.gnu.org/packages/")
               ("org" . "https://orgmode.org/elpa/"))
    :priorities (("melpa-cn" . 10)
                 ("gnu-cn" . 9)
                 ("nongnu-cn" . 8)
                 ("melpa" . 7)
                 ("gnu" . 6)
                 ("org" . 5))
    :timeout 60
    :retry-attempts 5
    :connection-timeout 30)
  "包管理配置常量")

;; ==================== 包源配置 ====================
(defun my/setup-package-archives ()
  "配置包源"
  (message "配置包源...")
  (setq package-archives (plist-get my/package-config :archives)
        package-archive-priorities (plist-get my/package-config :priorities)
        package-check-signature 'allow-unsigned
        package-enable-at-startup nil)
  (message "✓ 包源配置完成"))

;; ==================== 网络优化 ====================
(defun my/optimize-network-settings ()
  "优化网络设置"
  (message "优化网络设置...")
  (setq dotspacemacs-elpa-timeout (plist-get my/package-config :timeout)
        url-retry-attempts (plist-get my/package-config :retry-attempts)
        url-connection-timeout (plist-get my/package-config :connection-timeout)
        url-keepalive t)
  (message "✓ 网络设置优化完成"))

;; ==================== 智能包安装 ====================
(defun my/smart-package-install (package &optional retry-count)
  "智能包安装，自动选择最佳镜像源"
  (let ((retries (or retry-count 3))
        (success nil))
    (while (and (> retries 0) (not success))
      (message "尝试安装包: %s (剩余重试: %d)" package retries)
      (condition-case err
          (progn
            (package-install package)
            (setq success t)
            (message "✓ 包 %s 安装成功" package))
        (error
         (message "✗ 包 %s 安装失败: %s" package (error-message-string err))
         (setq retries (1- retries))
         (when (> retries 0)
           (sleep-for 2)))))
    success))

;; ==================== 包版本兼容性处理 ====================
(defun my/fix-package-version-compatibility ()
  "修复包版本兼容性问题"
  (message "检查包版本兼容性...")
  
  ;; 处理关键包安装
  (let ((critical-packages '(spinner auctex mathjax)))
    (dolist (pkg critical-packages)
      (when (not (package-installed-p pkg))
        (condition-case err
            (progn
              (package-refresh-contents)
              (package-install pkg)
              (message "✓ %s 包安装成功" pkg))
          (error
           (message "✗ %s 包安装失败: %s" pkg (error-message-string err))))))))

;; ==================== 依赖包修复 ====================
(defun my/fix-missing-dependencies ()
  "修复缺失的依赖包"
  (message "开始修复缺失的依赖包...")
  
  ;; 修复版本兼容性问题
  (my/fix-package-version-compatibility)
  
  ;; 安装其他依赖包
  (let ((other-packages '(lsp-mode lsp-ui company)))
    (dolist (pkg other-packages)
      (when (not (package-installed-p pkg))
        (my/smart-package-install pkg)))))

;; ==================== 智能包安装策略 ====================
(defun my/smart-package-install-strategy ()
  "智能包安装策略"
  (message "执行智能包安装策略...")
  
  ;; 设置镜像源和网络
  (my/setup-package-archives)
  (my/optimize-network-settings)
  
  ;; 初始化包管理器
  (package-initialize)
  (when (not package-archive-contents)
    (package-refresh-contents))
  
  ;; 修复依赖包
  (my/fix-missing-dependencies)
  
  (message "智能包安装策略完成！"))

;; ==================== 包清理和重置 ====================
(defun my/clean-package-cache ()
  "清理包缓存"
  (message "清理包缓存...")
  (let ((cache-dir (expand-file-name "elpa" user-emacs-directory)))
    (when (file-exists-p cache-dir)
      (delete-directory cache-dir t)
      (message "✓ 包缓存已清理"))))

(defun my/reset-package-archives ()
  "重置包源"
  (message "重置包源...")
  (setq package-archives nil
        package-archive-contents nil)
  (my/setup-package-archives)
  (message "✓ 包源已重置"))

;; ==================== 初始化 ====================
(defun my/init-package ()
  "初始化包管理模块"
  (message "初始化包管理模块...")
  (my/smart-package-install-strategy)
  (message "包管理模块初始化完成"))

;; 立即初始化
(my/init-package)

(provide 'package)
;;; package.el ends here
