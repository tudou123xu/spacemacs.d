;;; cursor-optimization.el --- Cursor编辑器优化管理 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== Cursor进程监控 ====================
(defun my/monitor-cursor-processes ()
  "监控Cursor进程状态"
  (interactive)
  (message "监控Cursor进程...")
  
  (let ((cursor-processes (my/get-cursor-processes)))
    (if cursor-processes
        (progn
          (message "发现 %d 个Cursor进程:" (length cursor-processes))
          (dolist (process cursor-processes)
            (let ((pid (nth 0 process))
                  (cpu (nth 1 process))
                  (mem (nth 2 process))
                  (name (nth 3 process)))
              (message "  PID: %s, CPU: %s%%, MEM: %sMB, %s" 
                       pid cpu mem name)))
          
          ;; 检查高CPU进程
          (let ((high-cpu-processes (seq-filter (lambda (p) (> (string-to-number (nth 1 p)) 20)) cursor-processes)))
            (when high-cpu-processes
              (message "⚠ 发现高CPU进程，建议优化"))))
      (message "✓ 未发现Cursor进程"))))

(defun my/get-cursor-processes ()
  "获取Cursor进程列表"
  (let ((output (shell-command-to-string "ps aux | grep -i cursor | grep -v grep | awk '{print $2, $3, $4, $11}'")))
    (when (> (length output) 0)
      (mapcar (lambda (line)
                (split-string line " " t))
              (split-string output "\n" t)))))

;; ==================== Cursor性能优化 ====================
(defun my/optimize-cursor-performance ()
  "优化Cursor性能"
  (interactive)
  (message "优化Cursor性能...")
  
  ;; 终止高CPU进程
  (my/kill-cursor-gpu-process)
  
  ;; 清理缓存
  (my/cleanup-cursor-cache)
  
  ;; 重启Cursor（可选）
  (when (y-or-n-p "是否重启Cursor以应用优化？")
    (my/restart-cursor))
  
  (message "✓ Cursor性能优化完成"))

(defun my/kill-cursor-gpu-process ()
  "终止Cursor GPU进程"
  (interactive)
  (message "终止Cursor GPU进程...")
  
  (let ((result (shell-command-to-string "killall 'Cursor Helper (GPU)' 2>/dev/null")))
    (if (= (length result) 0)
        (message "✓ GPU进程已终止")
      (message "⚠ GPU进程终止失败: %s" result))))

(defun my/cleanup-cursor-cache ()
  "清理Cursor缓存"
  (interactive)
  (message "清理Cursor缓存...")
  
  (let ((cache-dirs '("~/Library/Application Support/Cursor/logs"
                      "~/Library/Application Support/Cursor/CachedData"
                      "~/Library/Application Support/Cursor/User/workspaceStorage")))
    (dolist (dir cache-dirs)
      (let ((expanded-dir (expand-file-name dir)))
        (when (file-directory-p expanded-dir)
          (delete-directory expanded-dir t)
          (message "✓ 清理缓存: %s" expanded-dir))))))

(defun my/restart-cursor ()
  "重启Cursor"
  (interactive)
  (message "重启Cursor...")
  
  ;; 终止所有Cursor进程
  (shell-command "killall Cursor 2>/dev/null")
  (shell-command "killall 'Cursor Helper' 2>/dev/null")
  
  ;; 等待进程终止
  (sleep-for 2)
  
  ;; 启动Cursor
  (shell-command "open -a Cursor")
  (message "✓ Cursor已重启"))

;; ==================== Cursor配置管理 ====================
(defun my/configure-cursor-settings ()
  "配置Cursor设置"
  (interactive)
  (message "配置Cursor设置...")
  
  (let ((config-dir (expand-file-name "~/Library/Application Support/Cursor/User"))
        (settings-file (expand-file-name "settings.json" config-dir)))
    
    ;; 创建配置目录
    (make-directory config-dir t)
    
    ;; 写入优化配置
    (with-temp-file settings-file
      (insert (my/get-cursor-optimized-settings)))
    
    (message "✓ Cursor配置已更新: %s" settings-file)))

(defun my/get-cursor-optimized-settings ()
  "获取Cursor优化设置"
  (concat
   "{\n"
   "    // 性能优化配置\n"
   "    \"workbench.enableExperiments\": false,\n"
   "    \"workbench.settings.enableNaturalLanguageSearch\": false,\n"
   "    \"workbench.startupEditor\": \"none\",\n"
   "    \n"
   "    // 禁用不必要的功能\n"
   "    \"workbench.enableTelemetry\": false,\n"
   "    \"telemetry.telemetryLevel\": \"off\",\n"
   "    \n"
   "    // 编辑器性能优化\n"
   "    \"editor.minimap.enabled\": false,\n"
   "    \"editor.smoothScrolling\": false,\n"
   "    \"editor.cursorBlinking\": \"solid\",\n"
   "    \"editor.cursorSmoothCaretAnimation\": \"off\",\n"
   "    \"editor.occurrencesHighlight\": false,\n"
   "    \"editor.selectionHighlight\": false,\n"
   "    \"editor.hover.delay\": 1000,\n"
   "    \"editor.quickSuggestionsDelay\": 500,\n"
   "    \n"
   "    // 文件监控优化\n"
   "    \"files.watcherExclude\": {\n"
   "        \"**/.git/objects/**\": true,\n"
   "        \"**/node_modules/**\": true,\n"
   "        \"**/tmp/**\": true,\n"
   "        \"**/.DS_Store\": true\n"
   "    },\n"
   "    \n"
   "    // 搜索优化\n"
   "    \"search.followSymlinks\": false,\n"
   "    \"search.useIgnoreFiles\": true,\n"
   "    \n"
   "    // 扩展优化\n"
   "    \"extensions.autoUpdate\": false,\n"
   "    \"extensions.autoCheckUpdates\": false,\n"
   "    \n"
   "    // 终端优化\n"
   "    \"terminal.integrated.gpuAcceleration\": \"off\",\n"
   "    \"terminal.integrated.smoothScrolling\": false,\n"
   "    \n"
   "    // 内存优化\n"
   "    \"files.hotExit\": \"off\",\n"
   "    \"workbench.editor.enablePreview\": false,\n"
   "    \n"
   "    // 禁用动画\n"
   "    \"workbench.reduceMotion\": \"on\",\n"
   "    \"editor.suggest.animate\": false\n"
   "}\n"))

;; ==================== Cursor诊断工具 ====================
(defun my/diagnose-cursor-issues ()
  "诊断Cursor问题"
  (interactive)
  (message "诊断Cursor问题...")
  
  (let ((issues '())
        (warnings '()))
    
    ;; 检查高CPU进程
    (let ((high-cpu-processes (seq-filter (lambda (p) (> (string-to-number (nth 1 p)) 20))
                                          (my/get-cursor-processes))))
      (when high-cpu-processes
        (push (format "高CPU进程: %d个" (length high-cpu-processes)) issues)))
    
    ;; 检查内存使用
    (let ((high-mem-processes (seq-filter (lambda (p) (> (string-to-number (nth 2 p)) 2))
                                          (my/get-cursor-processes))))
      (when high-mem-processes
        (push (format "高内存进程: %d个" (length high-mem-processes)) warnings)))
    
    ;; 检查配置文件
    (let ((config-file (expand-file-name "~/Library/Application Support/Cursor/User/settings.json")))
      (unless (file-exists-p config-file)
        (push "缺少优化配置文件" warnings)))
    
    ;; 显示结果
    (if issues
        (progn
          (message "发现 %d 个问题:" (length issues))
          (dolist (issue issues)
            (message "  ⚠ %s" issue)))
      (message "✓ 未发现严重问题"))
    
    (when warnings
      (message "警告:")
      (dolist (warning warnings)
        (message "  ⚠ %s" warning)))
    
    (when (or issues warnings)
      (message "运行 M-x my/optimize-cursor-performance 来优化"))))

;; ==================== 快速优化命令 ====================
(defun my/quick-cursor-optimize ()
  "快速优化Cursor"
  (interactive)
  (message "快速优化Cursor...")
  
  ;; 终止GPU进程
  (my/kill-cursor-gpu-process)
  
  ;; 清理缓存
  (my/cleanup-cursor-cache)
  
  (message "✓ 快速优化完成"))

;; ==================== 提供模块 ====================
(provide 'cursor-optimization)
;;; cursor-optimization.el ends here
