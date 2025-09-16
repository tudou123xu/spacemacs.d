#!/usr/bin/env emacs --script
;;; health-check.el --- Spacemacs 配置健康检查 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 加载核心模块 ====================
(add-to-list 'load-path "~/.spacemacs.d/modules/")
(load-file "~/.spacemacs.d/modules/core-performance.el")
(load-file "~/.spacemacs.d/modules/error-handling.el")

;; ==================== 健康检查配置 ====================
(defvar health-check-results '()
  "健康检查结果列表")

(defun health/add-result (category status message)
  "添加检查结果"
  (push (list category status message) health-check-results))

;; ==================== 基础系统检查 ====================
(defun health/check-basic-system ()
  "检查基础系统状态"
  (message "检查基础系统...")
  
  ;; 检查 Emacs 版本
  (let ((version (emacs-version)))
    (if (version<= "26.1" emacs-version)
        (health/add-result "系统" "✓" (format "Emacs 版本: %s" version))
      (health/add-result "系统" "⚠" (format "Emacs 版本较低: %s" version))))
  
  ;; 检查图形界面
  (if (display-graphic-p)
      (health/add-result "系统" "✓" "图形界面可用")
    (health/add-result "系统" "⚠" "仅终端模式"))
  
  ;; 检查系统类型
  (let ((system-type-str (symbol-name system-type)))
    (health/add-result "系统" "✓" (format "系统类型: %s" system-type-str))))

;; ==================== 模块加载检查 ====================
(defun health/check-modules ()
  "检查模块加载状态"
  (message "检查模块加载...")
  
  (let ((modules '("core-performance" "error-handling" "package-fix"
                   "system-integration" "ui-enhancement" "lang-support" 
                   "security-audit" "macos-specific" "windows-specific")))
    (dolist (module modules)
      (let ((module-path (expand-file-name (concat "modules/" module ".el") "~/.spacemacs.d/")))
        (if (file-exists-p module-path)
            (health/add-result "模块" "✓" (format "%s 模块文件存在" module))
          (health/add-result "模块" "✗" (format "%s 模块文件缺失" module)))))))

;; ==================== 函数可用性检查 ====================
(defun health/check-functions ()
  "检查关键函数是否可用"
  (message "检查函数可用性...")
  
  (let ((required-functions '("my/number-of-processors"
                              "my/system-is-mac"
                              "my/system-is-windows"
                              "my/load-config-module")))
    (dolist (func required-functions)
      (if (fboundp (intern func))
          (health/add-result "函数" "✓" (format "%s 已定义" func))
        (health/add-result "函数" "✗" (format "%s 未定义" func))))))

;; ==================== 包管理检查 ====================
(defun health/check-package-management ()
  "检查包管理状态"
  (message "检查包管理...")
  
  ;; 检查包归档
  (if (and (boundp 'package-archives) package-archives)
      (health/add-result "包管理" "✓" (format "配置了 %d 个包归档" (length package-archives)))
    (health/add-result "包管理" "⚠" "未配置包归档"))
  
  ;; 检查包签名验证
  (if (and (boundp 'package-check-signature) (eq package-check-signature 'allow-unsigned))
      (health/add-result "包管理" "⚠" "允许未签名包")
    (health/add-result "包管理" "✓" "包签名验证已启用")))

;; ==================== 性能配置检查 ====================
(defun health/check-performance ()
  "检查性能配置"
  (message "检查性能配置...")
  
  ;; 检查 GC 设置
  (if (and (boundp 'gc-cons-threshold) 
           (> gc-cons-threshold 100000000))
      (health/add-result "性能" "✓" "GC 阈值已优化")
    (health/add-result "性能" "⚠" "GC 阈值未优化"))
  
  ;; 检查进程输出缓冲区
  (if (and (boundp 'read-process-output-max)
           (>= read-process-output-max (* 2 1024 1024)))
      (health/add-result "性能" "✓" "LSP 缓冲区已优化")
    (health/add-result "性能" "⚠" "LSP 缓冲区未优化")))

;; ==================== 安全配置检查 ====================
(defun health/check-security ()
  "检查安全配置"
  (message "检查安全配置...")
  
  ;; 检查审计日志
  (let ((log-file (expand-file-name "logs/config-audit.log" user-emacs-directory)))
    (if (file-exists-p log-file)
        (let ((perms (file-modes log-file)))
          (if (= perms #o600)
              (health/add-result "安全" "✓" "审计日志权限正确")
            (health/add-result "安全" "⚠" "审计日志权限不正确")))
      (health/add-result "安全" "⚠" "审计日志文件不存在"))))

;; ==================== 平台专属检查 ====================
(defun health/check-platform-specific ()
  "检查平台专属配置"
  (message "检查平台专属配置...")
  
  (cond
   ((my/system-is-mac)
    (let ((macos-file (expand-file-name "modules/macos-specific.el" "~/.spacemacs.d/")))
      (if (file-exists-p macos-file)
          (health/add-result "平台" "✓" "macOS 专属配置已加载")
        (health/add-result "平台" "✗" "macOS 专属配置缺失"))))
   
   ((my/system-is-windows)
    (let ((windows-file (expand-file-name "modules/windows-specific.el" "~/.spacemacs.d/")))
      (if (file-exists-p windows-file)
          (health/add-result "平台" "✓" "Windows 专属配置已加载")
        (health/add-result "平台" "✗" "Windows 专属配置缺失"))))
   
   (t
    (health/add-result "平台" "✓" "通用平台配置"))))

;; ==================== 主健康检查函数 ====================
(defun health/run-all-checks ()
  "运行所有健康检查"
  (interactive)
  (setq health-check-results '())
  
  (message "开始 Spacemacs 配置健康检查...")
  
  ;; 运行各项检查
  (health/check-basic-system)
  (health/check-modules)
  (health/check-functions)
  (health/check-package-management)
  (health/check-performance)
  (health/check-security)
  (health/check-platform-specific)
  
  ;; 显示结果
  (health/display-results))

(defun health/display-results ()
  "显示健康检查结果"
  (let ((total (length health-check-results))
        (passed (length (seq-filter (lambda (x) (string= (cadr x) "✓")) health-check-results)))
        (warnings (length (seq-filter (lambda (x) (string= (cadr x) "⚠")) health-check-results)))
        (failed (length (seq-filter (lambda (x) (string= (cadr x) "✗")) health-check-results))))
    
    (message "\n==================== 健康检查结果 ====================")
    (message "总计: %d | 通过: %d | 警告: %d | 失败: %d" total passed warnings failed)
    (message "")
    
    ;; 按类别分组显示结果
    (let ((categories '("系统" "模块" "函数" "包管理" "性能" "安全" "平台")))
      (dolist (category categories)
        (let ((category-results (seq-filter (lambda (x) (string= (car x) category)) health-check-results)))
          (when category-results
            (message "【%s】" category)
            (dolist (result category-results)
              (message "  %s %s" (cadr result) (caddr result)))
            (message "")))))
    
    ;; 总体评估
    (cond
     ((= failed 0)
      (message "🎉 配置状态: 健康")
      (when (> warnings 0)
        (message "💡 建议: 处理 %d 个警告以优化配置" warnings)))
     ((< failed 3)
      (message "⚠️  配置状态: 基本可用，建议修复 %d 个问题" failed))
     (t
      (message "❌ 配置状态: 需要修复 %d 个问题" failed)))
    
    (message "==================== 检查完成 ====================")))

;; ==================== 快速状态检查 ====================
(defun health/quick-status ()
  "快速状态检查"
  (interactive)
  (let ((critical-errors 0))
    ;; 检查关键文件
    (dolist (file '("init.el" "user-config.el" "modules/core-performance.el"))
      (unless (file-exists-p (expand-file-name file "~/.spacemacs.d/"))
        (setq critical-errors (1+ critical-errors))))
    
    ;; 检查关键函数
    (unless (fboundp 'my/load-config-module)
      (setq critical-errors (1+ critical-errors)))
    
    ;; 显示状态
    (cond
     ((= critical-errors 0)
      (message "✅ Spacemacs 配置状态: 正常"))
     ((< critical-errors 3)
      (message "⚠️  Spacemacs 配置状态: 基本可用 (%d 个问题)" critical-errors))
     (t
      (message "❌ Spacemacs 配置状态: 需要修复 (%d 个问题)" critical-errors)))))

;; ==================== 运行检查 ====================
(health/run-all-checks)

(provide 'health-check)
;;; health-check.el ends here