;;; user-config.el --- 增强版开发环境配置 -*- lexical-binding: t; -*-

;; ==================== 系统核心检测 (前置定义) ====================
(defun number-of-processors ()
  "动态获取处理器核心数（适配 macOS/Linux/Windows）"
  (let ((default-threads 4))
    (cond
     ((and (spacemacs/system-is-mac) (executable-find "/usr/sbin/sysctl"))  ;; 网页4的精准检测方法
      (string-to-number (shell-command-to-string "sysctl -n hw.logicalcpu")))  ;; 逻辑核心数[4](@ref)
     ((executable-find "nproc")  ;; Linux 系统
      (string-to-number (shell-command-to-string "nproc")))
     ((eq system-type 'windows-nt)  ;; Windows 系统
      (string-to-number (getenv "NUMBER_OF_PROCESSORS")))
     (t default-threads))))  ;; 默认值保障稳定性


;; ==================== 核心环境配置 ====================
;; 参考网页2的延迟加载策略[2](@ref)
(setq package-archives '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
;; 安装依赖包
(unless (package-installed-p 'ob-go)
  (package-refresh-contents)
  (package-install 'ob-go))

;; ==================== Org-Babel 智能配置 ====================
(use-package org
  :defer 2  ;; 延迟加载提升启动速度[2](@ref)
  :config
  (progn
    ;; 安全执行策略（网页5的安全审计思想[5](@ref)）
    (setq org-confirm-babel-evaluate
          (lambda (lang body)
            (unless (member lang '("python" "go"))
              (y-or-n-p (format "Execute %s code? " lang)))))

    ;; 动态语言支持（网页1的按需加载模式[1](@ref)）
    (defun my/org-babel-init ()
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((python . t)
         (java  . t)
         (go    . t)
         (R     . t)
         (shell . t)
         (js    . t)))
      (setq org-plantuml-jar-path
            (or (locate-file "plantuml.jar" (list "~/.emacs.d/lib/"))
                (warn "PlantUML jar file not found!"))))
    (add-hook 'org-mode-hook #'my/org-babel-init)

    ;; 智能解释器选择（网页3的路径检测增强[3](@ref)）
    (setq org-babel-python-command
          (cond ((executable-find "poetry") "poetry run python")
                ((executable-find "pipenv") "pipenv run python")
                (t "python3"))
          org-babel-go-command "go run -mod=readonly")  ;; 模块验证[3](@ref)

    ;; 统一头参数模板（网页1的配置规范化[1](@ref)）
    (dolist (lang '(python go java js))
      (setf (alist-get lang org-babel-default-header-args)
            '((:results . "output")
              (:session . "none")
              (:exports . "code")
              (:cache   . "yes"))))

    ;; 异步执行优化（网页4的线程池设计[4](@ref)）
    (setq org-babel-async-allow-in-python t
          org-babel-async-max-threads (number-of-processors)
          org-babel-async-timeout 30)

    ;; LSP深度集成（网页5的智能补全增强[5](@ref)）
    (with-eval-after-load 'lsp-mode
      (setq aider-code-suggestions-prompt
            "基于项目文档和以下代码上下文进行补全:\n%s"))))

;; ==================== 语言支持模块 ====================
;; Python开发环境（网页3的虚拟环境检测[3](@ref)）
(use-package python
  :defer t
  :config
  (progn
    (setq python-shell-interpreter "python3"
          python-shell-virtualenv-root "~/.pyenv/versions/"
          python-shell-completion-native-disabled-interpreters '("python3"))

    (defun my/python-virtualenv-detect ()
      (when-let ((venv (or (getenv "VIRTUAL_ENV")
                           (locate-dominating-file default-directory ".python-version"))))
        (setq python-shell-virtualenv-path venv)))
    (add-hook 'python-mode-hook #'my/python-virtualenv-detect)))

;; Go语言工具链（网页3的模块验证增强[3](@ref)）
(use-package go-mode
  :if (executable-find "go")
  :config
  (setq go-command "go1.22"
        gofmt-command "goimports"
        compile-command "go build -v && go test -v"))

;; ==================== 系统集成优化 ====================
;; 跨平台环境同步（网页1的路径管理策略[1](@ref)）
(use-package exec-path-from-shell
  :ensure t
  :config
  (progn
    (setq exec-path-from-shell-variables '("PATH" "PYTHONPATH" "GOPATH"))
    (exec-path-from-shell-initialize)
    (setenv "NODE_PATH" "/Users/xuzhifeng/.nvm/versions/node/v15.14.0/bin")))

;; ==================== 界面与交互优化 ====================
;; 跨平台字体渲染（网页3的增强方案[3](@ref)）
(when window-system
  (cond ((spacemacs/system-is-mac)
         (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16))
        ((spacemacs/system-is-mswindows)
         (dolist (charset '(kana han symbol cjk-misc bopomofo))
           (set-fontset-font t charset (font-spec :family "Microsoft Yahei" :size 14))))))
;; ==================== macOS 专用优化 ====================
(when (spacemacs/system-is-mac)
  ;; 字体渲染增强
  (set-fontset-font t 'han (font-spec :family "PingFang SC" :size 14))

  ;; 解决 Homebrew Python 路径问题
  (setq exec-path-from-shell-arguments '("-l"))  ;; 加载完整的登录shell环境
  (exec-path-from-shell-initialize))

;; 增强版词典查询（网页1的快捷键优化[1](@ref)）
(use-package youdao-dictionary
  :bind ("C-c y" . youdao-dictionary-search-at-point+))

;; ==================== 系统级优化 ====================
;; Windows性能增强（网页5的特殊处理[5](@ref)）
(when (spacemacs/system-is-mswindows)
  (progn
    (setq find-file-hook nil
          vc-handled-backends nil
          tramp-use-ssh-controlmaster nil
          tramp-ssh-controlmaster-options
          "-o ControlMaster=auto -o ControlPath=~/.ssh/ssh-%%r@%%h:%%p -o ControlPersist=60")

    ;; 智能路径处理（网页1的emax集成[1](@ref)）
    (when-let ((emax-dir (expand-file-name "~/emax/")))
      (when (file-exists-p emax-dir)
        (setq exec-path (append (list (concat emax-dir "bin64")
                                      (concat emax-dir "mingw64/bin"))
                                exec-path)
              ispell-program-name "aspell"))))

  ;; ==================== 安全审计模块 ====================
  ;; 配置变更追踪（网页5的审计机制[5](@ref)）
  (defvar my/config-audit-log "~/.emacs.d/config-audit.log")
  (defun my/log-config-change ()
    (when (string= (buffer-file-name) (expand-file-name "user-config.el" user-emacs-directory))
      (append-to-file (format "[%s] Modified by %s\n"
                              (format-time-string "%Y-%m-%d %H:%M:%S")
                              (user-real-login-name))
                      nil my/config-audit-log)))
  (add-hook 'after-save-hook #'my/log-config-change)

  (provide 'user-config)
;;; user-config.el ends here
