;;; quick-fix.el --- 一键修复 Spacemacs 配置问题 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 一键修复主函数 ====================
(defun my/quick-fix-all ()
  "一键修复所有配置问题"
  (interactive)
  (message "开始一键修复 Spacemacs 配置...")
  
  ;; 1. 修复包安装问题
  (my/fix-package-errors)
  
  ;; 2. 优化启动速度
  (my/optimize-startup-speed)
  
  ;; 3. 修复镜像源问题
  (my/fix-mirror-sources)
  
  ;; 4. 清理和优化
  (my/cleanup-and-optimize)
  
  (message "✓ 一键修复完成！请重启 Emacs 以应用更改"))

;; ==================== 修复包安装错误 ====================
(defun my/fix-package-errors ()
  "修复包安装错误"
  (message "修复包安装错误...")
  
  ;; 设置优化的包源
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
          ("org" . 5)))
  
  ;; 初始化包管理器
  (package-initialize)
  
  ;; 刷新包列表
  (condition-case err
      (progn
        (package-refresh-contents)
        (message "✓ 包列表刷新成功"))
    (error
     (message "⚠ 包列表刷新失败，使用缓存版本")))
  
  ;; 安装关键包
  (let ((critical-packages '(spinner auctex lsp-mode lsp-ui company)))
    (dolist (pkg critical-packages)
      (my/smart-install-package pkg))))

;; ==================== 智能包安装 ====================
(defun my/smart-install-package (package)
  "智能安装包"
  (when (not (package-installed-p package))
    (message "安装包: %s" package)
    (condition-case err
        (progn
          (package-install package)
          (message "✓ %s 安装成功" package))
      (error
       (message "✗ %s 安装失败: %s" package (error-message-string err))
       (my/try-alternative-install package)))))

(defun my/try-alternative-install (package)
  "尝试替代安装方法"
  (cond
   ((eq package 'spinner)
    (my/install-spinner-alternative))
   ((eq package 'auctex)
    (my/install-auctex-alternative))
   (t
    (message "⚠ 无法为 %s 找到替代安装方法" package))))

(defun my/install-spinner-alternative ()
  "安装 spinner 替代方案"
  (condition-case err
      (progn
        (package-install 'spinner :version "1.7.2")
        (message "✓ spinner 1.7.2 安装成功"))
    (error
     (message "✗ spinner 替代方案也失败"))))

(defun my/install-auctex-alternative ()
  "安装 auctex 替代方案"
  (condition-case err
      (progn
        (package-install 'auctex :version "11.87")
        (message "✓ auctex 11.87 安装成功"))
    (error
     (message "✗ auctex 替代方案也失败"))))

;; ==================== 优化启动速度 ====================
(defun my/optimize-startup-speed ()
  "优化启动速度"
  (message "优化启动速度...")
  
  ;; 优化启动设置
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil
        inhibit-splash-screen t
        inhibit-startup-buffer-menu t
        package-enable-at-startup nil
        package-quickstart nil
        package--init-file-ensured t)
  
  ;; 优化 GC 设置
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)
  
  ;; 优化字体设置
  (setq inhibit-compacting-font-caches t
        font-lock-maximum-decoration 1
        font-lock-verbose nil)
  
  ;; 优化文件处理
  (setq file-name-handler-alist nil
        vc-handled-backends nil
        find-file-hook nil
        auto-save-default nil
        make-backup-files nil)
  
  (message "✓ 启动速度优化完成"))

;; ==================== 修复镜像源问题 ====================
(defun my/fix-mirror-sources ()
  "修复镜像源问题"
  (message "修复镜像源配置...")
  
  ;; 设置网络超时
  (setq dotspacemacs-elpa-timeout 60
        url-retry-attempts 5
        url-connection-timeout 30
        url-keepalive t)
  
  ;; 禁用更新检查以减少网络请求
  (setq dotspacemacs-check-for-update nil
        dotspacemacs-update-check-remote-version nil)
  
  (message "✓ 镜像源配置完成"))

;; ==================== 清理和优化 ====================
(defun my/cleanup-and-optimize ()
  "清理和优化"
  (message "清理和优化...")
  
  ;; 清理包缓存
  (let ((cache-dir (expand-file-name "elpa" user-emacs-directory)))
    (when (and (file-exists-p cache-dir)
               (> (length (directory-files cache-dir t)) 10))  ; 如果缓存文件过多
      (message "清理包缓存...")
      (delete-directory cache-dir t)
      (message "✓ 包缓存已清理")))
  
  ;; 清理临时文件
  (let ((temp-dir (expand-file-name "logs" user-emacs-directory)))
    (when (file-exists-p temp-dir)
      (mapc (lambda (file)
              (when (and (file-regular-p file)
                         (> (- (float-time) (file-attribute-modification-time (file-attributes file)))
                            86400))  ; 删除超过1天的文件
                (delete-file file)))
            (directory-files temp-dir t))))
  
  ;; 优化消息日志
  (setq message-log-max 1000
        warning-minimum-level :error)
  
  (message "✓ 清理和优化完成"))

;; ==================== 诊断工具 ====================
(defun my/diagnose-issues ()
  "诊断当前问题"
  (interactive)
  (message "诊断 Spacemacs 配置问题...")
  
  (let ((issues '())
        (warnings '()))
    
    ;; 检查包安装
    (when (not (package-installed-p 'spinner))
      (push "spinner 包未安装" issues))
    (when (not (package-installed-p 'auctex))
      (push "auctex 包未安装" issues))
    (when (not (package-installed-p 'lsp-mode))
      (push "lsp-mode 包未安装" issues))
    
    ;; 检查配置模块
    (when (not (file-exists-p "~/.spacemacs.d/modules/core-performance.el"))
      (push "核心性能模块缺失" issues))
    (when (not (file-exists-p "~/.spacemacs.d/modules/package-fix.el"))
      (push "包修复模块缺失" issues))
    
    ;; 检查网络连接
    (when (not (my/test-network))
      (push "网络连接问题" warnings))
    
    ;; 显示结果
    (if issues
        (progn
          (message "发现 %d 个问题:" (length issues))
          (dolist (issue issues)
            (message "  ✗ %s" issue)))
      (message "✓ 未发现严重问题"))
    
    (when warnings
      (message "警告:")
      (dolist (warning warnings)
        (message "  ⚠ %s" warning)))
    
    (when (or issues warnings)
      (message "运行 M-x my/quick-fix-all 来自动修复问题"))))

(defun my/test-network ()
  "测试网络连接"
  (condition-case nil
      (progn
        (url-retrieve-synchronously "https://melpa.org" nil nil 5)
        t)
    (error nil)))

;; ==================== 提供模块 ====================
(provide 'quick-fix)
;;; quick-fix.el ends here
