;;; database.el --- 数据库管理模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 包定义 ====================
(defconst database-packages
  '(sql sql-indent sqlup-mode sqlformat)
  "数据库相关包列表")

;; ==================== 配置 ====================
(defvar database/config-table (make-hash-table :test 'equal)
  "数据库配置表")

(defun database/init-database ()
  "初始化数据库模块"
  (database/load-db-profiles)
  (database/setup-keybindings))

(defun database/load-db-profiles ()
  "从配置文件加载数据库配置"
  (let ((config-file "~/org/db-config.org"))
    (when (file-exists-p config-file)
      (org-babel-load-file config-file)
      (dolist (profile '("mysql-prod" "pg-staging"))
        (puthash profile (org-babel-ref-resolve profile) database/config-table)))))

;; ==================== 连接功能 ====================
(defun database/connect (profile)
  "连接到指定数据库"
  (interactive (list (completing-read "选择数据库:" (hash-table-keys database/config-table))))
  (let* ((params (gethash profile database/config-table))
         (type (plist-get params :type)))
    (pcase type
      ('mysql (database/mysql-connect params))
      ('postgres (database/pgsql-connect params))
      (_ (message "不支持的数据库类型: %s" type)))))

(defun database/mysql-connect (params)
  "连接 MySQL 数据库"
  (sql-connect
   (format "%s-%s" (plist-get params :user) (plist-get params :database))
   (plist-get params :user)
   (plist-get params :password)
   (plist-get params :database)
   (plist-get params :host)
   (plist-get params :port)))

(defun database/pgsql-connect (params)
  "连接 PostgreSQL 数据库"
  (sql-connect
   (format "postgresql-%s" (plist-get params :database))
   (plist-get params :user)
   (plist-get params :password)
   (plist-get params :database)
   (plist-get params :host)
   (plist-get params :port)))

;; ==================== 查询功能 ====================
(defun database/execute-query (query)
  "执行 SQL 查询"
  (interactive "s输入 SQL 查询: ")
  (when (sql-get-product) (sql-send-string query)))

(defun database/format-query ()
  "格式化当前 SQL 查询"
  (interactive)
  (when (eq major-mode 'sql-mode) (sqlformat-buffer)))

(defun database/uppercase-keywords ()
  "将 SQL 关键字转换为大写"
  (interactive)
  (when (eq major-mode 'sql-mode)
    (sqlup-capitalize-keywords-in-region (point-min) (point-max))))

;; ==================== 管理功能 ====================
(defun database/list-tables ()
  "列出数据库表"
  (interactive)
  (database/execute-query "SHOW TABLES;"))

(defun database/describe-table (table-name)
  "描述表结构"
  (interactive "s输入表名: ")
  (database/execute-query (format "DESCRIBE %s;" table-name)))

(defun database/show-databases ()
  "显示所有数据库"
  (interactive)
  (database/execute-query "SHOW DATABASES;"))

;; ==================== 快捷键配置 ====================
(defun database/setup-keybindings ()
  "配置数据库快捷键"
  (when (fboundp 'spacemacs/set-leader-keys)
    (spacemacs/set-leader-keys
      "odc"  #'database/connect
      "odq"  #'database/execute-query
      "odf"  #'database/format-query
      "odu"  #'database/uppercase-keywords
      "odl"  #'database/list-tables
      "odd"  #'database/describe-table
      "ods"  #'database/show-databases)
    
    (when (fboundp 'spacemacs/set-leader-keys-for-major-mode)
      (spacemacs/set-leader-keys-for-major-mode 'sql-mode
        "cq"   #'database/execute-query
        "cf"   #'database/format-query
        "cu"   #'database/uppercase-keywords
        "cl"   #'database/list-tables
        "cd"   #'database/describe-table)))
  
  (global-set-key (kbd "C-c d c") #'database/connect)
  (global-set-key (kbd "C-c d q") #'database/execute-query)
  (global-set-key (kbd "C-c d f") #'database/format-query))

;; ==================== 初始化 ====================
(database/init-database)

(provide 'database)
;;; database.el ends here
