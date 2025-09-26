;;; config-manager.el --- 分层配置管理器 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 配置层定义 ====================
(defconst my/config-layers
  '((early-init
     :path ""
     :modules ("early-init")
     :priority 0
     :required t)
    
    (foundation
     :path "utils/"
     :modules ("common")
     :priority 1
     :required t
     :dependencies (early-init))
    
    (core
     :path "core/"
     :modules ("performance" "package" "error-handling")
     :priority 2
     :required t
     :dependencies (foundation))
    
    (system
     :path "system/"
     :modules ("integration")
     :priority 3
     :required t
     :dependencies (foundation core))
    
    (features
     :path "features/"
     :modules ("ui-enhancement" "lang-support" "security" "ai-assistant" "database")
     :priority 4
     :required nil
     :dependencies (core)))
  "配置层定义")

;; ==================== 层加载器 ====================
(defun my/load-layer (layer-name)
  "加载指定配置层"
  (unless (my/layer-loaded-p layer-name)
    (let* ((layer-def (assoc layer-name my/config-layers))
           (path (plist-get (cdr layer-def) :path))
           (modules (plist-get (cdr layer-def) :modules))
           (required (plist-get (cdr layer-def) :required))
           (dependencies (plist-get (cdr layer-def) :dependencies)))
      
      (when layer-def
        (message "加载配置层: %s" layer-name)
        
        ;; 加载依赖
        (dolist (dep dependencies)
          (my/load-layer dep))
        
        ;; 加载模块
        (dolist (module modules)
          (let ((module-path (if (string-empty-p path)
                                 (expand-file-name (concat module ".el") "~/.spacemacs.d/")
                               (expand-file-name (concat module ".el") 
                                               (expand-file-name path "~/.spacemacs.d/")))))
            (if (my/load-module-file module-path)
                (message "  ✓ 模块 %s 加载成功" module)
              (when required
                (message "  ✗ 必需模块 %s 加载失败" module)))))
        
        (push layer-name my/loaded-layers)
        (message "✓ 配置层 %s 加载完成" layer-name)))))

;; ==================== 层状态检查 ====================
(defvar my/loaded-layers '()
  "已加载的配置层列表")

(defun my/layer-loaded-p (layer-name)
  "检查配置层是否已加载"
  (member layer-name my/loaded-layers))

(defun my/load-module-file (module-path)
  "安全加载模块文件"
  (when (file-exists-p module-path)
    (condition-case err
        (progn
          (load-file module-path)
          (message "✓ 模块 %s 加载成功" (file-name-nondirectory module-path))
          t)
      (error
       (message "✗ 模块 %s 加载失败: %s" (file-name-nondirectory module-path) (error-message-string err))
       nil))))

;; ==================== 智能加载 ====================
(defun my/load-config-layers ()
  "智能加载所有配置层"
  (message "开始加载配置层...")
  
  ;; 按优先级排序并加载必需层
  (dolist (layer (sort (copy-sequence my/config-layers)
                       (lambda (a b) (< (plist-get (cdr a) :priority)
                                        (plist-get (cdr b) :priority)))))
    (let ((layer-name (car layer))
          (required (plist-get (cdr layer) :required)))
      (when required
        (my/load-layer layer-name))))
  
  ;; 延迟加载可选层
  (run-with-timer 1 nil
                 (lambda ()
                   (dolist (layer my/config-layers)
                     (let ((layer-name (car layer))
                           (required (plist-get (cdr layer) :required)))
                       (unless required
                         (my/load-layer layer-name))))))
  
  (message "✓ 配置层加载完成"))

(defun my/load-all-layers ()
  "加载所有配置层"
  (interactive)
  (message "开始加载所有配置层...")
  (setq my/loaded-layers '())
  (dolist (layer (sort (copy-sequence my/config-layers)
                       (lambda (a b) (< (plist-get (cdr a) :priority)
                                        (plist-get (cdr b) :priority)))))
    (my/load-layer (car layer)))
  (message "所有配置层加载完成"))

;; ==================== 配置验证 ====================
(defun my/validate-configuration ()
  "验证配置完整性"
  (interactive)
  (message "验证配置完整性...")
  
  (let ((required-layers '("early-init" "foundation" "core" "system"))
        (missing-layers (seq-filter (lambda (layer) (not (my/layer-loaded-p layer)))
                                    required-layers)))
    
    (if missing-layers
        (message "警告: 缺少关键配置层: %s" (string-join missing-layers ", "))
      (message "✓ 配置验证通过"))))

;; ==================== 配置管理接口 ====================
(defun my/show-config-status ()
  "显示配置状态"
  (interactive)
  (message "配置层状态:")
  (dolist (layer my/config-layers)
    (let ((layer-name (car layer))
          (loaded (my/layer-loaded-p layer-name)))
      (message "  %s: %s" layer-name (if loaded "已加载" "未加载"))))
  
  (message "已加载层数: %d/%d" (length my/loaded-layers) (length my/config-layers)))

;; ==================== 初始化 ====================
(defun my/init-config-manager ()
  "初始化配置管理器"
  (message "初始化配置管理器...")
  
  ;; 加载所有配置层
  (my/load-config-layers)
  
  ;; 延迟验证
  (run-with-timer 2 nil #'my/validate-configuration)
  
  (message "配置管理器初始化完成"))

;; 立即初始化
(my/init-config-manager)

(provide 'config-manager)
;;; config-manager.el ends here
