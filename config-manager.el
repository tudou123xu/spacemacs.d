;;; config-manager.el --- 分层配置管理器 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024
;; Description: 提供模块化的配置层管理，支持依赖解析和延迟加载

;; ==================== 变量定义 ====================
(defvar my/loaded-layers '()
  "已加载的配置层列表")

(defvar my/layer-load-errors '()
  "层加载错误记录")

;; ==================== 配置层定义 ====================
(defconst my/config-layers
  '((early-init
     :path ""
     :modules ("early-init")
     :priority 0
     :required t
     :description "早期初始化配置")
    
    (foundation
     :path "utils/"
     :modules ("common")
     :priority 1
     :required t
     :dependencies (early-init)
     :description "基础工具模块")
    
    (core
     :path "core/"
     :modules ("performance" "package" "error-handling")
     :priority 2
     :required t
     :dependencies (foundation)
     :description "核心功能模块")
    
    (system
     :path "system/"
     :modules ("integration")
     :priority 3
     :required t
     :dependencies (foundation core)
     :description "系统集成模块")
    
    (features
     :path "features/"
     :modules ("ui-enhancement" "lang-support" "security" "ai-assistant" "database")
     :priority 4
     :required nil
     :dependencies (core)
     :description "功能扩展模块"))
  "配置层定义，包含优先级和依赖关系")

;; ==================== 层加载器 ====================
(defun my/load-layer (layer-name)
  "加载指定配置层，处理依赖关系
参数:
  LAYER-NAME - 层名称（symbol）"
  (unless (my/layer-loaded-p layer-name)
    (let* ((layer-def (assoc layer-name my/config-layers))
           (path (plist-get (cdr layer-def) :path))
           (modules (plist-get (cdr layer-def) :modules))
           (required (plist-get (cdr layer-def) :required))
           (dependencies (plist-get (cdr layer-def) :dependencies))
           (description (plist-get (cdr layer-def) :description)))
      
      (if (not layer-def)
          (progn
            (message "✗ 未找到配置层定义: %s" layer-name)
            (push (cons layer-name "层定义不存在") my/layer-load-errors))
        
        (message "加载配置层: %s (%s)" layer-name (or description ""))
        
        ;; 加载依赖
        (dolist (dep dependencies)
          (unless (my/layer-loaded-p dep)
            (my/load-layer dep)))
        
        ;; 加载模块
        (let ((all-success t))
          (dolist (module modules)
            (let ((module-path (if (string-empty-p path)
                                   (expand-file-name (concat module ".el") "~/.spacemacs.d/")
                                 (expand-file-name (concat module ".el") 
                                                 (expand-file-name path "~/.spacemacs.d/")))))
              (if (my/load-module-file module-path)
                  (message "  ✓ 模块 %s 加载成功" module)
                (progn
                  (setq all-success nil)
                  (when required
                    (message "  ✗ 必需模块 %s 加载失败" module)
                    (push (cons layer-name (format "模块 %s 加载失败" module)) 
                          my/layer-load-errors))))))
          
          (when all-success
            (push layer-name my/loaded-layers)
            (message "✓ 配置层 %s 加载完成" layer-name)))))))

;; ==================== 层状态检查 ====================
(defun my/layer-loaded-p (layer-name)
  "检查配置层是否已加载
参数:
  LAYER-NAME - 层名称（symbol）"
  (member layer-name my/loaded-layers))

(defun my/load-module-file (module-path)
  "安全加载模块文件，带错误处理
参数:
  MODULE-PATH - 模块文件路径（string）
返回:
  t - 加载成功
  nil - 加载失败"
  (if (not (file-exists-p module-path))
      (progn
        (message "✗ 模块文件不存在: %s" module-path)
        nil)
    (condition-case err
        (progn
          (load-file module-path)
          t)
      (error
       (message "✗ 模块 %s 加载失败: %s" 
                (file-name-nondirectory module-path) 
                (error-message-string err))
       (when (require 'error-handling nil t)
         (my/log-error (format "模块加载失败: %s" module-path)
                       (error-message-string err)))
       nil))))

;; ==================== 智能加载 ====================
(defun my/load-config-layers ()
  "智能加载所有配置层，按优先级加载必需层，延迟加载可选层"
  (message "开始加载配置层...")
  
  ;; 按优先级排序并加载必需层
  (let ((sorted-layers (sort (copy-sequence my/config-layers)
                            (lambda (a b) 
                              (< (plist-get (cdr a) :priority)
                                 (plist-get (cdr b) :priority))))))
    
    (dolist (layer sorted-layers)
      (let ((layer-name (car layer))
            (required (plist-get (cdr layer) :required)))
        (when required
          (my/load-layer layer-name)))))
  
  ;; 延迟加载可选层
  (run-with-timer 1 nil
                 (lambda ()
                   (message "开始延迟加载可选层...")
                   (dolist (layer my/config-layers)
                     (let ((layer-name (car layer))
                           (required (plist-get (cdr layer) :required)))
                       (unless required
                         (my/load-layer layer-name))))
                   (message "✓ 可选层加载完成")))
  
  (message "✓ 配置层加载完成"))

(defun my/load-all-layers ()
  "加载所有配置层（包括可选层）"
  (interactive)
  (message "开始加载所有配置层...")
  (setq my/loaded-layers '()
        my/layer-load-errors '())
  
  (let ((sorted-layers (sort (copy-sequence my/config-layers)
                            (lambda (a b) 
                              (< (plist-get (cdr a) :priority)
                                 (plist-get (cdr b) :priority))))))
    (dolist (layer sorted-layers)
      (my/load-layer (car layer))))
  
  (if my/layer-load-errors
      (progn
        (message "⚠ 配置层加载完成，但有 %d 个错误" (length my/layer-load-errors))
        (my/show-load-errors))
    (message "✓ 所有配置层加载完成")))

;; ==================== 配置验证 ====================
(defun my/validate-configuration ()
  "验证配置完整性，检查必需层是否加载"
  (interactive)
  (message "验证配置完整性...")
  
  (let ((required-layers (mapcar #'car
                                (seq-filter (lambda (layer)
                                             (plist-get (cdr layer) :required))
                                           my/config-layers))))
    
    (let ((missing-layers (seq-filter (lambda (layer) 
                                       (not (my/layer-loaded-p layer)))
                                     required-layers)))
      
      (if missing-layers
          (progn
            (message "⚠ 警告: 缺少关键配置层: %s" 
                    (string-join (mapcar #'symbol-name missing-layers) ", "))
            (when (require 'error-handling nil t)
              (my/log-warning "配置验证失败" 
                             (format "缺少层: %s" missing-layers)))
            nil)
        (message "✓ 配置验证通过")
        t))))

(defun my/show-load-errors ()
  "显示加载错误列表"
  (interactive)
  (if my/layer-load-errors
      (progn
        (message "\n========== 配置加载错误 ==========")
        (dolist (error my/layer-load-errors)
          (message "  层 %s: %s" (car error) (cdr error)))
        (message "=================================\n"))
    (message "无加载错误")))

;; ==================== 配置管理接口 ====================
(defun my/show-config-status ()
  "显示配置状态，包括加载情况和描述"
  (interactive)
  (message "\n========== 配置层状态 ==========")
  (dolist (layer my/config-layers)
    (let ((layer-name (car layer))
          (description (plist-get (cdr layer) :description))
          (loaded (my/layer-loaded-p layer-name)))
      (message "  %s: %s - %s" 
               layer-name 
               (if loaded "✓ 已加载" "✗ 未加载")
               (or description ""))))
  
  (message "\n统计: 已加载 %d/%d 层" 
           (length my/loaded-layers) 
           (length my/config-layers))
  
  (when my/layer-load-errors
    (message "错误数: %d" (length my/layer-load-errors)))
  
  (message "=================================\n"))

(defun my/reload-layer (layer-name)
  "重新加载指定配置层
参数:
  LAYER-NAME - 层名称（symbol）"
  (interactive
   (list (intern (completing-read "选择要重新加载的层: "
                                   (mapcar (lambda (l) (symbol-name (car l)))
                                          my/config-layers)
                                   nil t))))
  
  ;; 从已加载列表中移除
  (setq my/loaded-layers (delq layer-name my/loaded-layers))
  
  ;; 重新加载
  (my/load-layer layer-name)
  
  (message "✓ 配置层 %s 已重新加载" layer-name))

;; ==================== 初始化 ====================
(defun my/init-config-manager ()
  "初始化配置管理器"
  (message "初始化配置管理器...")
  
  ;; 重置状态
  (setq my/loaded-layers '()
        my/layer-load-errors '())
  
  ;; 加载所有配置层
  (my/load-config-layers)
  
  ;; 延迟验证配置
  (run-with-timer 2 nil 
                 (lambda ()
                   (my/validate-configuration)
                   (when my/layer-load-errors
                     (message "⚠ 配置加载有错误，运行 M-x my/show-load-errors 查看详情"))))
  
  (message "✓ 配置管理器初始化完成"))

;; 立即初始化
(my/init-config-manager)

(provide 'config-manager)
;;; config-manager.el ends here
