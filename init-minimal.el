;;; init-minimal.el --- 优化的最小化启动配置 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 国内镜像源配置 ====================
(setq package-archives
      '(("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("gnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

;; ==================== 启动优化配置 ====================
(defun dotspacemacs/layers ()
  "优化的最小化层配置"
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(;; 核心层（必需）
     better-defaults
     helm
     emacs-lisp
     (ivy :variables ivy-enable-advanced-buffer-information t)
     (spell-checking :variables spell-checking-enable-by-default nil))
   
   dotspacemacs-additional-packages '()
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "优化的初始化配置"
  (setq-default
   ;; 基础设置
   dotspacemacs-enable-emacs-pdumper nil
   dotspacemacs-emacs-pdumper-executable-file "emacs"
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)
   
   ;; 网络优化
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 60
   url-retry-attempts 5
   url-connection-timeout 30
   url-keepalive t
   
   ;; 性能优化
   dotspacemacs-gc-cons '(134217728 0.1)
   dotspacemacs-read-process-output-max (* 3 1024 1024)
   dotspacemacs-use-spacelpa nil
   dotspacemacs-verify-spacelpa-archives t
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory 'emacs-version
   
   ;; 界面设置
   dotspacemacs-editing-style 'emacs
   dotspacemacs-startup-buffer-show-version t
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner-scale 'auto
   dotspacemacs-startup-lists '((recents . 5))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-show-startup-list-numbers t
   dotspacemacs-startup-buffer-multi-digit-delay 0.4
   dotspacemacs-startup-buffer-show-icons nil
   
   ;; 缓冲区设置
   dotspacemacs-new-empty-buffer-major-mode 'text-mode
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-scratch-buffer-persistent nil
   dotspacemacs-scratch-buffer-unkillable nil
   dotspacemacs-initial-scratch-message nil
   
   ;; 主题设置
   dotspacemacs-themes '(spacemacs-dark spacemacs-light)
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("SF Mono" :size 14.0 :weight normal :width normal)
   
   ;; 键位设置
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")
   dotspacemacs-distinguish-gui-tab nil
   
   ;; 布局设置
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-generate-layout-names nil
   
   ;; 文件设置
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   
   ;; 交互设置
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-loading-progress-bar t
   
   ;; 全屏设置
   dotspacemacs-fullscreen-at-startup t
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-undecorated-at-startup nil
   
   ;; 透明度设置
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-background-transparency 90
   
   ;; 状态设置
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-scroll-bar-while-scrolling t
   
   ;; 编辑设置
   dotspacemacs-line-numbers t
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-activate-smartparens-mode t
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   
   ;; 服务器设置
   dotspacemacs-enable-server nil
   dotspacemacs-server-socket-dir nil
   dotspacemacs-persistent-server nil
   
   ;; 搜索设置
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   
   ;; 标题设置
   dotspacemacs-frame-title-format "%I@%S"
   dotspacemacs-icon-title-format nil
   
   ;; 其他设置
   dotspacemacs-show-trailing-whitespace t
   dotspacemacs-whitespace-cleanup nil
   dotspacemacs-use-clean-aindent-mode t
   dotspacemacs-use-SPC-as-y nil
   dotspacemacs-swap-number-row nil
   dotspacemacs-zone-out-when-idle nil
   dotspacemacs-pretty-docs nil
   dotspacemacs-home-shorten-agenda-source nil
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "环境变量设置"
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "用户初始化"
  ;; 设置国内镜像源
  (setq package-archives
        '(("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ("gnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ("nongnu-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))
  
  ;; 网络优化
  (setq url-retry-attempts 5
        url-connection-timeout 30
        url-keepalive t))

(defun dotspacemacs/user-load ()
  "加载库"
  (add-to-list 'load-path (file-truename "~/.spacemacs.d/")))

(defun dotspacemacs/user-config ()
  "用户配置"
  (message "使用优化的最小化配置启动"))

(provide 'init-minimal)
;;; init-minimal.el ends here