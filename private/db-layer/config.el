;; ~/.spacemacs.d/private/db-layer/config.el
(defvar my/db-config-table (make-hash-table :test 'equal))

(defun my/load-db-profiles ()
  "从Org文件加载数据库配置"
  (org-babel-load-file "~/org/db-config.org")
  (dolist (profile '("mysql-prod" "pg-staging"))
    (puthash profile (org-babel-ref-resolve profile) my/db-config-table)))

(defun my/db-connect (profile)
  (interactive (list (completing-read "选择数据库:" (hash-table-keys my/db-config-table))))
  (let* ((params (gethash profile my/db-config-table))
         (type (plist-get params :type)))
    (pcase type
      ('mysql (my/mysql-query params))
      ('postgres (my/pgsql-query params)))))

(defun my/mysql-query (params)
  (sql-connect
   (format "%s-%s" (plist-get params :user) (plist-get params :database))
   (plist-get params :user)
   (plist-get params :password)
   (plist-get params :database)
   (plist-get params :host)
   (plist-get params :port)))

(defun my/pgsql-query (params)
  (sql-connect
   (format "postgresql-%s" (plist-get params :database))
   (plist-get params :user)
   (plist-get params :password)
   (plist-get params :database)
   (plist-get params :host)
   (plist-get params :port)))
