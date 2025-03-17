;;配置 Babel 默认参数
;; ==================== Org Babel 增强配置 ====================
(with-eval-after-load 'org
  ;; 代码块执行免确认
  (setq org-confirm-babel-evaluate nil)

  ;; 多语言支持配置
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (java  . t)
     (go    . t)
     (R     . t)
     (shell . t)
     (js    . t)))

  ;; 智能 Python 解释器选择
  (setq org-babel-python-command
        (or (executable-find "python3")  ;; 自动检测系统路径
            "python"))  ;; 保底方案
  ;; 代码块头参数模板
  (setq org-babel-default-header-args:python
        '((:results . "output")
          (:session . "none")
          (:exports . "code")))

  ;; 异步执行支持
  (setq org-babel-async-allow-in-python t)
  ;; PlantUML 路径动态配置
  (setq org-plantuml-jar-path
        (expand-file-name "~/.emacs.d/lib/plantuml.jar")))

;; ==================== python 语言支持优化 ====================
(unless (package-installed-p 'ob-python)
  (package-refresh-contents)
  (package-install 'ob-python))
;; 同步 Shell 环境变量（解决 /opt/homebrew/bin/python3 路径问题）
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables '("PATH" "PYTHONPATH"))
  (exec-path-from-shell-initialize))
;; ==================== Go 语言支持优化 ====================
(unless (package-installed-p 'ob-go)
  (package-refresh-contents)
  (package-install 'ob-go))

;; ==================== Python 开发环境强化 ====================
(with-eval-after-load 'python
  (setq python-shell-interpreter "python3"
        python-shell-completion-native-disabled-interpreters '("python3")  ;; 强制使用 LSP
        python-shell-virtualenv-root "~/.pyenv/versions/"))  ;; 虚拟环境支持
;;有道快捷键r
(define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)
;;解决org表格里面中英文对齐的问题
(when (configuration-layer/layer-usedp 'chinese)
  (when (and (spacemacs/system-is-mac) window-system)
    (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16)))

;; enable org-protocol
(server-start t)
;;代码org mod
(require 'org-tempo)
(require 'org-protocol)
(require 'org-roam-protocol)
(setq ispell-program-name "aspell")
(setq ispell-dictionary "english")

;; Setting Chinese Font
(when (and (spacemacs/system-is-mswindows) window-system)
  (setq w32-pass-alt-to-system nil)
  (setq w32-apps-modifier 'super)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Microsoft Yahei" :size 14))))

;; boost find file and load saved persp layout  performance
;; which will break some function on windows platform
;; eg. known issues: magit related buffer color, reopen will fix it
(when (spacemacs/system-is-mswindows)
  (progn (setq find-file-hook nil)
         (setq vc-handled-backends nil)
         (setq magit-refresh-status-buffer nil)
         (add-hook 'find-file-hook 'spacemacs/check-large-file)

         ;; emax.7z in not under pdumper release
         ;; https://github.com/m-parashar/emax64/releases/tag/pdumper-20180619
         (defvar emax-root (concat (expand-file-name "~") "/emax"))

         (when (file-exists-p emax-root)
           (progn
             (defvar emax-root (concat (expand-file-name "~") "/emax"))
             (defvar emax-bin64 (concat emax-root "/bin64"))
             (defvar emax-mingw64 (concat emax-root "/mingw64/bin"))
             (defvar emax-lisp (concat emax-root "/lisp"))

             (setq exec-path (cons emax-bin64 exec-path))
             (setenv "PATH" (concat emax-bin64 ";" (getenv "PATH")))

             (setq exec-path (cons emax-mingw64 exec-path))
             (setenv "PATH" (concat emax-mingw64 ";" (getenv "PATH")))

             ;; install aspell: https://sheishe.xyz/post/using-aspell-in-windows-10-and-emacs-26-above/
             (add-to-list 'exec-path "C:/msys64/mingw64/bin/")
             (setq ispell-program-name "aspell")
             (setq ispell-personal-dictionary "c:/msys64/mingw64/lib/aspell-0.60/en_GB")

             ))

         (add-hook 'projectile-mode-hook '(lambda () (remove-hook 'find-file-hook #'projectile-find-file-hook-function)))))

(setq exec-path (cons "/Users/xuzhifeng/.nvm/versions/node/v15.14.0/bin/" exec-path))
(setenv "PATH" (concat "/Users/xuzhifeng/.nvm/versions/node/v15.14.0/bin:" (getenv "PATH")))
