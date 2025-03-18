(defun ellama/init-ellama ()
  (use-package ellama
    :init
    (setq ellama-provider 'ollama    ;; 使用本地 Ollama 服务
          ellama-model "llama3:8b"   ;; 默认模型
          ellama-api-base "http://localhost:11434")
    :config
    (spacemacs/set-leader-keys
      "oel" #'ellama-chat-new
      "oer" #'ellama-region-rewrite)))


;; 明文密码版 MySQL 连接函数
(defun my/mysql-tramp-connect (env)
  (interactive (list (completing-read "Env:" (mapcar 'car my/mysql-envs))))
  (let* ((params (cadr (assoc env my/mysql-envs)))
         (tramp-path (format "/ssh:%s:/" (plist-get params :host)))
         (cmd (format "mysql --host=%s --port=%d -u %s -p%s %s"
                      (plist-get params :host)
                      (plist-get params :port)
                      (plist-get params :user)
                      (plist-get params :password) ; 直接读取明文密码
                      (plist-get params :database))))
    (async-shell-command cmd)))

;; 简化版 PostgreSQL 连接
(defun my/psql-tramp-connect (cluster)
  (let* ((params (cadr (assoc cluster my/pg-clusters)))
         (conn-str (format "postgresql://%s:%s@%s:%d/%s"
                           (plist-get params :user)
                           (plist-get params :password) ; 明文密码注入
                           (plist-get params :host)
                           (plist-get params :port)
                           sql-database)))
    (setenv "PGPASSWORD" (plist-get params :password))
    (sql-connect conn-str)))

;; ~/.emacs.d/private/db-layer/config.el


;; 配置中心路径声明（参考网页5的环境变量思想）
(defvar my/db-config-root "~/org-notes/org-roam/config/db-config.org")

;; 安全校验机制
(unless (file-exists-p my/db-config-root)
  (make-directory my/db-config-root t)
  (warn "Created database config directory: %s" my/db-config-root))

;; 主加载函数（集成网页1的异步加载策略）
(defun my/load-db-config ()
  "动态加载Org格式数据库配置"
  (interactive)
  (require 'ob)
  (org-babel-load-file (expand-file-name "db-config.org" my/db-config-root))
  (message "Database config reloaded from %s" my/db-config-root))

;; 初始化加载
(my/load-db-config)

;; 热重载监控（参考网页5的自动更新机制）
(add-hook 'after-save-hook
          (lambda ()
            (when (string= (buffer-file-name)
                           (expand-file-name "db-config.org" my/db-config-root))
              (my/load-db-config))))
(defun my/select-db-env ()
  "交互式环境选择器"
  (interactive)
  (let ((env (completing-read "Environment: " '("dev" "prod" "staging"))))
    (setenv "DB_ENV" env)
    (my/load-db-config)))
(defvar my/db-connection-times (make-hash-table :test 'equal))

(defun my/log-db-latency (host duration)
  (puthash host (cons duration (gethash host my/db-connection-times))
           my/db-connection-times))
