;;; database.el --- 数据库管理模块 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024
;; Description: 提供数据库连接、查询和管理功能

;; ==================== 包定义 ====================
(defconst database-packages
  '(sql sql-indent sqlup-mode sqlformat)
  "数据库相关包列表")

;; ==================== 变量定义 ====================
(defvar database/config-table (make-hash-table :test 'equal)
  "数据库配置哈希表，存储各个数据库的连接信息")

;; ==================== 初始化 ====================
(defun database/init-database ()
  "初始化数据库模块"
  (message "初始化数据库模块...")
  (database/load-db-profiles)
  (database/setup-keybindings)
  (message "✓ 数据库模块初始化完成"))

(defun database/load-db-profiles ()
  "从配置文件加载数据库配置"
  (let ((config-file "~/org/db-config.org"))
    (when (file-exists-p config-file)
      (condition-case err
          (progn
            (org-babel-load-file config-file)
            (message "✓ 数据库配置已加载"))
        (error
         (message "✗ 数据库配置加载失败: %s" (error-message-string err)))))))

;; ==================== 连接功能 ====================
(defun database/connect (profile)
  "连接到指定的数据库配置
参数:
  PROFILE - 数据库配置名称"
  (interactive
   (list (completing-read "选择数据库: "
                          (hash-table-keys database/config-table)
                          nil t)))
  
  (let* ((params (gethash profile database/config-table))
         (type (plist-get params :type)))
    (if (not params)
        (message "✗ 未找到数据库配置: %s" profile)
      (pcase type
        ('mysql (database/mysql-connect params))
        ('postgres (database/pgsql-connect params))
        (_ (message "✗ 不支持的数据库类型: %s" type))))))

(defun database/mysql-connect (params)
  "连接 MySQL 数据库
参数:
  PARAMS - 数据库连接参数"
  (condition-case err
      (sql-connect
       (format "%s-%s" (plist-get params :user) (plist-get params :database))
       (plist-get params :user)
       (plist-get params :password)
       (plist-get params :database)
       (plist-get params :host)
       (plist-get params :port))
    (error
     (message "✗ MySQL 连接失败: %s" (error-message-string err)))))

(defun database/pgsql-connect (params)
  "连接 PostgreSQL 数据库
参数:
  PARAMS - 数据库连接参数"
  (condition-case err
      (sql-connect
       (format "postgresql-%s" (plist-get params :database))
       (plist-get params :user)
       (plist-get params :password)
       (plist-get params :database)
       (plist-get params :host)
       (plist-get params :port))
    (error
     (message "✗ PostgreSQL 连接失败: %s" (error-message-string err)))))

;; ==================== 查询功能 ====================
(defun database/execute-query (query)
  "执行 SQL 查询
参数:
  QUERY - SQL 查询语句"
  (interactive "sSQL 查询: ")
  (if (sql-get-product)
      (sql-send-string query)
    (message "✗ 未连接到数据库")))

(defun database/format-query ()
  "格式化当前缓冲区的 SQL 查询"
  (interactive)
  (if (eq major-mode 'sql-mode)
      (condition-case err
          (progn
            (sqlformat-buffer)
            (message "✓ SQL 格式化完成"))
        (error
         (message "✗ SQL 格式化失败: %s" (error-message-string err))))
    (message "⚠ 当前不是 SQL 模式")))

(defun database/uppercase-keywords ()
  "将 SQL 关键字转换为大写"
  (interactive)
  (if (eq major-mode 'sql-mode)
      (condition-case err
          (progn
            (sqlup-capitalize-keywords-in-region (point-min) (point-max))
            (message "✓ SQL 关键字已转换为大写"))
        (error
         (message "✗ 关键字转换失败: %s" (error-message-string err))))
    (message "⚠ 当前不是 SQL 模式")))

;; ==================== 管理功能 ====================
(defun database/list-tables ()
  "列出当前数据库的所有表"
  (interactive)
  (database/execute-query "SHOW TABLES;"))

(defun database/describe-table (table-name)
  "显示表的结构信息
参数:
  TABLE-NAME - 表名"
  (interactive "s表名: ")
  (database/execute-query (format "DESCRIBE %s;" table-name)))

(defun database/show-databases ()
  "显示所有数据库"
  (interactive)
  (database/execute-query "SHOW DATABASES;"))

;; ==================== 快捷键配置 ====================
(defun database/setup-keybindings ()
  "配置数据库模块的快捷键"
  ;; Spacemacs 风格快捷键
  (when (fboundp 'spacemacs/set-leader-keys)
    (spacemacs/set-leader-keys
      "odc" #'database/connect
      "odq" #'database/execute-query
      "odf" #'database/format-query
      "odu" #'database/uppercase-keywords
      "odl" #'database/list-tables
      "odd" #'database/describe-table
      "ods" #'database/show-databases)
    
    (when (fboundp 'spacemacs/set-leader-keys-for-major-mode)
      (spacemacs/set-leader-keys-for-major-mode 'sql-mode
        "cq" #'database/execute-query
        "cf" #'database/format-query
        "cu" #'database/uppercase-keywords
        "cl" #'database/list-tables
        "cd" #'database/describe-table)))
  
  ;; 全局快捷键
  (global-set-key (kbd "C-c d c") #'database/connect)
  (global-set-key (kbd "C-c d q") #'database/execute-query)
  (global-set-key (kbd "C-c d f") #'database/format-query)
  
  (message "✓ 数据库快捷键已配置"))

;; ==================== 自动初始化 ====================
(database/init-database)

(provide 'database)
;;; database.el ends here
