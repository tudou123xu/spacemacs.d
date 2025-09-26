/*
 * @Author: xuzhifeng xuzhifeng@hellogroup.com
 * @Date: 2025-09-22 11:20:13
 * @LastEditors: xuzhifeng xuzhifeng@hellogroup.com
 * @LastEditTime: 2025-09-26 15:19:13
 * @FilePath: /.spacemacs.d/core/package.el
 * @Description: 这是默认设置,请设置`customMade`, 打开koroFileHeader查看配置 进行设置: https://github.com/OBKoro1/koro1FileHeader/wiki/%E9%85%8D%E7%BD%AE
 */
;;; package.el --- 包管理核心模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

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
        package-check-signature 'allow-unsigned
        package-enable-at-startup nil)
  
  ;; 立即应用配置
  (when (fboundp 'package-initialize)
    (package-initialize)))

;; ==================== 网络优化 ====================
(defun my/optimize-network-settings ()
  "优化网络设置"
  (setq dotspacemacs-elpa-timeout 60
        url-retry-attempts 5
        url-connection-timeout 30
        url-keepalive t))

;; ==================== 智能包安装 ====================
(defun my/smart-package-install (package &optional version retry-count)
  "智能包安装，支持版本指定和重试机制"
  (let ((retries (or retry-count 3))
        (success nil)
        (package-name (if version 
                          (intern (concat (symbol-name package) "-" version))
                        package)))
    (while (and (> retries 0) (not success))
      (condition-case err
          (progn
            (package-install package-name)
            (setq success t)
            (message "✓ 包 %s 安装成功" package-name))
        (error
         (message "✗ 包 %s 安装失败: %s" package-name (error-message-string err))
         (setq retries (1- retries))
         (when (> retries 0) (sleep-for 2)))))
    success))

;; ==================== 包修复功能 ====================
(defun my/fix-missing-packages ()
  "修复所有缺失的包"
  (interactive)
  (message "开始修复缺失的包...")
  
  ;; 设置包源
  (my/setup-package-archives)
  
  ;; 刷新包列表
  (when (not package-archive-contents)
    (package-refresh-contents))
  
  ;; 修复关键包
  (my/fix-critical-packages)
  
  ;; 修复LSP相关包
  (my/fix-lsp-packages)
  
  (message "✓ 包修复完成！请重启 Emacs 以应用更改"))

(defun my/fix-critical-packages ()
  "修复关键包"
  (let ((critical-packages '((spinner . "1.7.2")
                             (auctex . "11.87")
                             (mathjax . "0.1"))))
    (dolist (pkg-spec critical-packages)
      (let ((pkg (car pkg-spec))
            (version (cdr pkg-spec)))
        (when (not (package-installed-p pkg))
          (my/smart-package-install pkg version))))))

(defun my/fix-lsp-packages ()
  "修复LSP相关包"
  (let ((lsp-packages '(lsp-mode lsp-ui company)))
    (dolist (pkg lsp-packages)
      (when (not (package-installed-p pkg))
        (my/smart-package-install pkg)))))

;; ==================== 包管理工具 ====================
(defun my/force-refresh-packages ()
  "强制刷新包列表"
  (interactive)
  (message "强制刷新包列表...")
  (my/setup-package-archives)
  (my/optimize-network-settings)
  (package-refresh-contents)
  (message "✓ 包列表刷新完成"))

(defun my/clean-package-cache ()
  "清理包缓存"
  (interactive)
  (let ((cache-dir (expand-file-name "elpa" user-emacs-directory)))
    (when (file-exists-p cache-dir)
      (delete-directory cache-dir t)
      (message "✓ 包缓存已清理"))))

(defun my/reset-package-archives ()
  "重置包源"
  (interactive)
  (setq package-archives nil
        package-archive-contents nil)
  (my/setup-package-archives)
  (message "✓ 包源已重置"))

;; ==================== 初始化 ====================
(defun my/init-package ()
  "初始化包管理模块"
  (message "初始化包管理器...")
  (my/setup-package-archives)
  (my/optimize-network-settings)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (message "✓ 包管理器初始化完成"))

;; 立即应用包源配置
(my/setup-package-archives)
(my/optimize-network-settings)

;; 延迟初始化（避免与 Spacemacs 初始化冲突）
(run-with-timer 2 nil #'my/init-package)

(provide 'package)
;;; package.el ends here
