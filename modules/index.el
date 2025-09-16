;;; index.el --- 模块索引和加载管理 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 模块定义 ====================
(defvar my/module-definitions
  '((core-performance
     :file "core-performance.el"
     :description "核心性能优化模块"
     :priority 1
     :required t
     :dependencies nil)
    
    (error-handling
     :file "error-handling.el"
     :description "错误处理和容错模块"
     :priority 2
     :required t
     :dependencies (core-performance))
    
    (package-fix
     :file "package-fix.el"
     :description "包依赖修复和网络优化"
     :priority 3
     :required t
     :dependencies (core-performance))
    
    (system-integration
     :file "system-integration.el"
     :description "系统集成模块"
     :priority 4
     :required t
     :dependencies (core-performance error-handling))
    
    (ui-enhancement
     :file "ui-enhancement.el"
     :description "界面增强模块"
     :priority 5
     :required nil
     :dependencies (core-performance))
    
    (lang-support
     :file "lang-support.el"
     :description "编程语言支持模块"
     :priority 6
     :required nil
     :dependencies (core-performance error-handling))
    
    (security-audit
     :file "security-audit.el"
     :description "安全审计模块"
     :priority 7
     :required nil
     :dependencies (error-handling))
    
    (macos-specific
     :file "macos-specific.el"
     :description "macOS 专属配置"
     :priority 8
     :required nil
     :dependencies (core-performance system-integration)
     :platform macos)
    
    (windows-specific
     :file "windows-specific.el"
     :description "Windows 专属配置"
     :priority 8
     :required nil
     :dependencies (core-performance system-integration)
     :platform windows))
  "模块定义列表")

;; ==================== 系统检测函数 ====================
(defun my/system-is-mac ()
  "检测是否为 macOS 系统"
  (eq system-type 'darwin))

(defun my/system-is-linux ()
  "检测是否为 Linux 系统"
  (eq system-type 'gnu/linux))

(defun my/system-is-windows ()
  "检测是否为 Windows 系统"
  (eq system-type 'windows-nt))

;; ==================== 模块加载函数 ====================
(defun my/load-config-module (module-name)
  "安全加载配置模块"
  (let ((module-file (expand-file-name (concat module-name ".el") my/config-modules-path)))
    (when (file-exists-p module-file)
      (condition-case err
          (progn
            (load-file module-file)
            (message "✓ 模块 %s 加载成功" module-name)
            t)
        (error 
         (message "✗ 模块 %s 加载失败: %s" module-name (error-message-string err))
         nil)))))

;; ==================== 平台过滤 ====================
(defun my/filter-platform-modules (modules)
  "根据平台过滤模块"
  (seq-filter (lambda (module)
                (let ((platform (plist-get (cdr module) :platform)))
                  (or (null platform)
                      (and (eq platform 'macos) (my/system-is-mac))
                      (and (eq platform 'windows) (my/system-is-windows))
                      (and (eq platform 'linux) (my/system-is-linux)))))
              modules))

;; ==================== 智能加载 ====================
(defun my/load-modules-smart (module-list)
  "智能加载模块列表"
  (let ((all-loaded '())
        (all-failed '())
        (platform-filtered (my/filter-platform-modules module-list)))
    
    ;; 按优先级排序
    (setq platform-filtered (sort platform-filtered 
                                   (lambda (a b)
                                     (< (plist-get (cdr a) :priority)
                                        (plist-get (cdr b) :priority)))))
    
    ;; 加载每个模块
    (dolist (module platform-filtered)
      (let ((module-name (car module)))
        (if (my/load-config-module module-name)
            (push module-name all-loaded)
          (push module-name all-failed))))
    
    (message "模块加载完成: %d 成功, %d 失败" 
             (length all-loaded) (length all-failed))
    
    (list :loaded all-loaded :failed all-failed)))

;; ==================== 模块状态检查 ====================
(defun my/check-module-status (module-name)
  "检查模块状态"
  (let ((module-def (assoc module-name my/module-definitions)))
    (if module-def
        (let ((file-path (expand-file-name (plist-get (cdr module-def) :file) my/config-modules-path)))
          (cond
           ((not (file-exists-p file-path))
            'missing)
           ((my/load-config-module module-name)
            'loaded)
           (t 'failed)))
      'undefined)))

;; ==================== 模块信息查询 ====================
(defun my/get-module-info (module-name)
  "获取模块信息"
  (let ((module-def (assoc module-name my/module-definitions)))
    (when module-def
      (list :name module-name
            :file (plist-get (cdr module-def) :file)
            :description (plist-get (cdr module-def) :description)
            :priority (plist-get (cdr module-def) :priority)
            :required (plist-get (cdr module-def) :required)
            :dependencies (plist-get (cdr module-def) :dependencies)
            :platform (plist-get (cdr module-def) :platform)))))

;; ==================== 模块列表显示 ====================
(defun my/list-modules ()
  "显示所有模块信息"
  (interactive)
  (let ((modules (my/filter-platform-modules my/module-definitions)))
    (message "可用模块 (%d 个):" (length modules))
    (dolist (module modules)
      (let ((info (my/get-module-info (car module))))
        (message "  %s: %s [优先级: %d]"
                 (plist-get info :name)
                 (plist-get info :description)
                 (plist-get info :priority))))))

;; ==================== 提供模块 ====================
(provide 'index)
;;; index.el ends here