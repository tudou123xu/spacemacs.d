;;; performance-optimization.el --- 系统性能优化配置 -*- lexical-binding: t; -*-
;; Author: xuzhifeng
;; Created: 2024

;; ==================== 系统性能监控 ====================
(defun my/monitor-system-performance ()
  "监控系统性能"
  (interactive)
  (message "监控系统性能...")
  
  ;; 检查CPU使用率
  (let ((cpu-usage (my/get-cpu-usage)))
    (message "CPU使用率: %s%%" cpu-usage)
    (when (> cpu-usage 80)
      (message "⚠ 警告: CPU使用率过高 (%s%%)" cpu-usage))))
  
  ;; 检查内存使用
  (let ((memory-usage (my/get-memory-usage)))
    (message "内存使用: %s MB" memory-usage)
    (when (> memory-usage 8000)
      (message "⚠ 警告: 内存使用过高 (%s MB)" memory-usage))))

(defun my/get-cpu-usage ()
  "获取CPU使用率"
  (let ((output (shell-command-to-string "top -l 1 | grep 'CPU usage' | awk '{print $3}' | sed 's/%//'")))
    (string-to-number (string-trim output))))

(defun my/get-memory-usage ()
  "获取内存使用量(MB)"
  (let ((output (shell-command-to-string "vm_stat | grep 'Pages active' | awk '{print $3}' | sed 's/\\.//'")))
    (* (string-to-number (string-trim output)) 4096 0.000001)))

;; ==================== Emacs性能优化 ====================
(defun my/optimize-emacs-performance ()
  "优化Emacs性能"
  (interactive)
  (message "优化Emacs性能...")
  
  ;; 垃圾回收优化
  (setq gc-cons-threshold 100000000)  ; 100MB
  (setq gc-cons-percentage 0.1)
  
  ;; 减少自动保存频率
  (setq auto-save-interval 300)  ; 5分钟
  (setq auto-save-timeout 30)    ; 30秒
  
  ;; 优化字体渲染
  (setq inhibit-compacting-font-caches t)
  
  ;; 减少不必要的钩子
  (remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
  
  ;; 优化LSP性能
  (when (boundp 'lsp-idle-delay)
    (setq lsp-idle-delay 0.5))
  
  (when (boundp 'lsp-log-io)
    (setq lsp-log-io nil))
  
  (message "✓ Emacs性能优化完成"))

;; ==================== 系统资源清理 ====================
(defun my/cleanup-system-resources ()
  "清理系统资源"
  (interactive)
  (message "清理系统资源...")
  
  ;; 强制垃圾回收
  (garbage-collect)
  
  ;; 清理Emacs缓存
  (when (fboundp 'recentf-cleanup)
    (recentf-cleanup))
  
  ;; 清理临时文件
  (let ((temp-dir (expand-file-name "temp" user-emacs-directory)))
    (when (file-directory-p temp-dir)
      (delete-directory temp-dir t)))
  
  (message "✓ 系统资源清理完成"))

;; ==================== 进程管理 ====================
(defun my/kill-high-cpu-processes ()
  "终止高CPU进程"
  (interactive)
  (message "终止高CPU进程...")
  
  (let ((high-cpu-processes '("Cursor Helper (GPU)" "Google Chrome Helper")))
    (dolist (process high-cpu-processes)
      (when (my/process-running-p process)
        (message "终止进程: %s" process)
        (shell-command (format "killall '%s' 2>/dev/null" process))))))
  
  (message "✓ 高CPU进程终止完成"))

(defun my/process-running-p (process-name)
  "检查进程是否运行"
  (let ((output (shell-command-to-string (format "pgrep -f '%s'" process-name))))
    (> (length (string-trim output)) 0)))

;; ==================== 性能诊断 ====================
(defun my/diagnose-performance-issues ()
  "诊断性能问题"
  (interactive)
  (message "诊断性能问题...")
  
  (let ((issues '()))
    
    ;; 检查CPU使用率
    (let ((cpu-usage (my/get-cpu-usage)))
      (when (> cpu-usage 80)
        (push (format "CPU使用率过高: %s%%" cpu-usage) issues)))
    
    ;; 检查内存使用
    (let ((memory-usage (my/get-memory-usage)))
      (when (> memory-usage 8000)
        (push (format "内存使用过高: %s MB" memory-usage) issues)))
    
    ;; 检查高CPU进程
    (let ((high-cpu-processes (my/get-high-cpu-processes)))
      (when high-cpu-processes
        (push (format "高CPU进程: %s" (string-join high-cpu-processes ", ")) issues)))
    
    ;; 显示结果
    (if issues
        (progn
          (message "发现 %d 个性能问题:" (length issues))
          (dolist (issue issues)
            (message "  ⚠ %s" issue))
          (message "运行 M-x my/optimize-emacs-performance 来优化"))
      (message "✓ 未发现性能问题"))))

(defun my/get-high-cpu-processes ()
  "获取高CPU进程列表"
  (let ((output (shell-command-to-string "ps aux | sort -k3 -nr | head -5 | awk '{print $2, $3, $11}'")))
    (split-string output "\n" t)))

;; ==================== 自动优化 ====================
(defun my/auto-optimize-performance ()
  "自动性能优化"
  (interactive)
  (message "自动性能优化...")
  
  ;; 检查性能问题
  (my/diagnose-performance-issues)
  
  ;; 优化Emacs
  (my/optimize-emacs-performance)
  
  ;; 清理资源
  (my/cleanup-system-resources)
  
  (message "✓ 自动性能优化完成"))

;; ==================== 提供模块 ====================
(provide 'performance-optimization)
;;; performance-optimization.el ends here
