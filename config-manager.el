;;; config-manager.el --- 分层配置管理器 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 配置层定义 ====================
(defconst my/config-layers
  '((foundation
     :path "utils/"
     :modules ("common")
     :priority 1
     :required t)
    
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
    
    (platform
     :path "system/platform/"
     :modules ("macos" "windows")
     :priority 4
     :required t
     :dependencies (system))
    
    (features
     :path "features/"
     :modules ("ui-enhancement" "lang-support" "security")
     :priority 5
     :required nil
     :dependencies (core))
    
    (services
     :path "scripts/"
     :modules ("config-manager" "quick-fix")
     :priority 6
     :required nil
     :dependencies (core)))
  "配置层定义")

;; ==================== 层加载器 ====================
(defun my/load-layer (layer-name)
  "加载指定配置层"
  (let ((layer-def (assoc layer-name my/config-layers)))
    (when layer-def
      (let ((path (plist-get (cdr layer-def) :path))
            (modules (plist-get (cdr layer-def) :modules))
            (required (plist-get (cdr layer-def) :required)))
        
        (message "加载配置层: %s" layer-name)
        
        ;; 检查依赖
        (let ((dependencies (plist-get (cdr layer-def) :dependencies)))
          (dolist (dep dependencies)
            (unless (my/layer-loaded-p dep)
              (my/load-layer dep))))
        
        ;; 加载模块
        (dolist (module modules)
          (let ((module-path (concat "~/.spacemacs.d/" path)))
            (if (my/load-config-module module module-path)
                (message "  ✓ 模块 %s 加载成功" module)
              (when required
                (message "  ✗ 必需模块 %s 加载失败" module)))))
        
        (setq my/loaded-layers (cons layer-name my/loaded-layers))
        (message "✓ 配置层 %s 加载完成" layer-name)))))

;; ==================== 层状态检查 ====================
(defvar my/loaded-layers '()
  "已加载的配置层列表")

(defun my/layer-loaded-p (layer-name)
  "检查配置层是否已加载"
  (member layer-name my/loaded-layers))

;; ==================== 智能加载 ====================
(defun my/load-config-layers ()
  "智能加载所有配置层"
  (message "开始加载配置层...")
  
  ;; 加载必需层
  (dolist (layer my/config-layers)
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

;; ==================== 配置验证 ====================
(defun my/validate-configuration ()
  "验证配置完整性"
  (message "验证配置完整性...")
  
  (let ((required-layers '("foundation" "core" "system" "platform"))
        (missing-layers '()))
    
    (dolist (layer required-layers)
      (unless (my/layer-loaded-p layer)
        (push layer missing-layers)))
    
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
      (message "  %s: %s" layer-name (if loaded "已加载" "未加载")))))

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
