#!/usr/bin/env emacs --script
;;; fix-packages.el --- 包依赖修复脚本 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

(require 'package)

;; 设置国内镜像源
(setq package-archives
      '(("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("gnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

;; 网络优化
(setq url-retry-attempts 5
      url-connection-timeout 30
      url-keepalive t)

;; 包安装函数
(defun install-package-with-retry (package-name &optional retry-count)
  "带重试机制的包安装"
  (let ((retries (or retry-count 3))
        (success nil))
    (while (and (> retries 0) (not success))
      (message "尝试安装包: %s (剩余重试: %d)" package-name retries)
      (condition-case err
          (progn
            (package-install package-name)
            (setq success t)
            (message "✓ 包 %s 安装成功" package-name))
        (error
         (message "✗ 包 %s 安装失败: %s" package-name (error-message-string err))
         (setq retries (1- retries))
         (when (> retries 0)
           (sleep-for 2)))))
    success))

;; 主修复函数
(defun main ()
  "主修复流程"
  (message "开始包依赖修复...")
  
  ;; 初始化包系统
  (package-initialize)
  
  ;; 刷新包列表
  (message "刷新包列表...")
  (condition-case err
      (progn
        (package-refresh-contents)
        (message "✓ 包列表刷新成功"))
    (error
     (message "✗ 包列表刷新失败: %s" (error-message-string err))))
  
  ;; 安装缺失的包
  (message "安装缺失的包...")
  (let ((missing-packages
         '(spinner auctex mathjax lsp-mode lsp-ui company)))
    (dolist (pkg missing-packages)
      (when (not (package-installed-p pkg))
        (install-package-with-retry pkg))))
  
  (message "包依赖修复完成！"))

;; 运行主函数
(main)