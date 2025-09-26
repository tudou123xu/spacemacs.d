/*
 * @Author: xuzhifeng xuzhifeng@hellogroup.com
 * @Date: 2025-09-22 11:20:13
 * @LastEditors: xuzhifeng xuzhifeng@hellogroup.com
 * @LastEditTime: 2025-09-23 11:10:34
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
(defun my/smart-package-install (package &optional retry-count)
  "智能包安装，自动选择最佳镜像源"
  (let ((retries (or retry-count 3))
        (success nil))
    (while (and (> retries 0) (not success))
      (condition-case err
          (progn
            (package-install package)
            (setq success t)
            (message "✓ 包 %s 安装成功" package))
        (error
         (message "✗ 包 %s 安装失败: %s" package (error-message-string err))
         (setq retries (1- retries))
         (when (> retries 0) (sleep-for 2)))))
    success))

(defun my/smart-package-install-with-version (package version &optional retry-count)
  "智能包安装，指定版本"
  (let ((retries (or retry-count 3))
        (success nil))
    (while (and (> retries 0) (not success))
      (condition-case err
          (progn
            (package-install (intern (concat (symbol-name package) "-" version)))
            (setq success t)
            (message "✓ 包 %s-%s 安装成功" package version))
        (error
         (message "✗ 包 %s-%s 安装失败: %s" package version (error-message-string err))
         (setq retries (1- retries))
         (when (> retries 0) (sleep-for 2)))))
    success))

;; ==================== 包版本兼容性处理 ====================
(defun my/fix-package-version-compatibility ()
  "修复包版本兼容性问题"
  (let ((critical-packages '((spinner . "1.7.2")
                             (auctex . "11.87")
                             (mathjax . "0.1"))))
    (dolist (pkg-spec critical-packages)
      (let ((pkg (car pkg-spec))
            (version (cdr pkg-spec)))
        (when (not (package-installed-p pkg))
          (my/smart-package-install-with-version pkg version))))))

;; ==================== 依赖包修复 ====================
(defun my/fix-missing-dependencies ()
  "修复缺失的依赖包"
  (my/fix-package-version-compatibility)
  (let ((other-packages '(lsp-mode lsp-ui company)))
    (dolist (pkg other-packages)
      (when (not (package-installed-p pkg))
        (my/smart-package-install (symbol-name pkg))))))

;; ==================== 包安装策略 ====================
(defun my/smart-package-install-strategy ()
  "智能包安装策略"
  (condition-case err
      (progn
        (my/setup-package-archives)
        (my/optimize-network-settings)
        (package-initialize)
        (when (not package-archive-contents)
          (package-refresh-contents))
        (my/fix-missing-dependencies))
    (error
     (message "包安装策略执行失败: %s" (error-message-string err)))))

;; ==================== 启动时包初始化 ====================
(defun my/init-package-on-startup ()
  "启动时初始化包管理器"
  (message "初始化包管理器...")
  (my/setup-package-archives)
  (my/optimize-network-settings)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (message "✓ 包管理器初始化完成"))

;; ==================== 包管理工具 ====================
(defun my/force-refresh-packages ()
  "强制刷新包列表"
  (interactive)
  (message "强制刷新包列表...")
  (my/setup-package-archives)
  (my/optimize-network-settings)
  (package-refresh-contents)
  (message "✓ 包列表刷新完成"))

;; ==================== 包清理和重置 ====================
(defun my/clean-package-cache ()
  "清理包缓存"
  (let ((cache-dir (expand-file-name "elpa" user-emacs-directory)))
    (when (file-exists-p cache-dir)
      (delete-directory cache-dir t)
      (message "✓ 包缓存已清理"))))

(defun my/reset-package-archives ()
  "重置包源"
  (setq package-archives nil
        package-archive-contents nil)
  (my/setup-package-archives))

;; ==================== 初始化 ====================
(defun my/init-package ()
  "初始化包管理模块"
  (my/smart-package-install-strategy))

;; 立即应用包源配置
(my/setup-package-archives)
(my/optimize-network-settings)

;; 延迟初始化（避免与 Spacemacs 初始化冲突）
(run-with-timer 2 nil #'my/init-package-on-startup)

;; 立即初始化
(my/init-package)

(provide 'package)
;;; package.el ends here
