;;; package-fix.el --- 包依赖修复和网络优化配置 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 国内镜像源配置 ====================
(defun my/setup-china-mirrors ()
  "配置国内镜像源，提高下载速度"
  (setq package-archives
        '(("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ("gnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ("nongnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
          ("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/"))))

;; ==================== 网络优化配置 ====================
(defun my/optimize-network-settings ()
  "优化网络设置，提高连接速度"
  (setq
   dotspacemacs-elpa-timeout 60
   url-retry-attempts 5
   url-connection-timeout 30
   url-keepalive t))

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

;; ==================== 依赖包修复 ====================
(defun my/fix-missing-dependencies ()
  "修复缺失的依赖包"
  (let ((missing-packages
         '(spinner auctex mathjax lsp-mode lsp-ui company)))
    (message "开始修复缺失的依赖包...")
    (dolist (pkg missing-packages)
      (when (not (package-installed-p pkg))
        (my/smart-package-install pkg)))))

;; ==================== 启动优化 ====================
(defun my/optimize-startup ()
  "优化启动速度"
  (message "优化 Emacs 启动...")
  (my/setup-china-mirrors)
  (my/optimize-network-settings)
  (package-initialize)
  (package-refresh-contents)
  (my/fix-missing-dependencies)
  (message "启动优化完成！"))

(provide 'package-fix)
;;; package-fix.el ends here