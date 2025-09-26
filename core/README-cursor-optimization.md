# Cursor编辑器性能优化指南

## 🚀 快速开始

### 方法一：使用Emacs命令（推荐）

```elisp
;; 监控Cursor进程
M-x my/monitor-cursor-processes

;; 快速优化Cursor
M-x my/quick-cursor-optimize

;; 完整优化Cursor
M-x my/optimize-cursor-performance

;; 诊断Cursor问题
M-x my/diagnose-cursor-issues

;; 配置Cursor设置
M-x my/configure-cursor-settings
```

### 方法二：使用命令行脚本

```bash
# 运行完整优化
~/.spacemacs.d/scripts/optimize-cursor.sh

# 使用进程管理脚本
~/.cursor-manager.sh status      # 查看状态
~/.cursor-manager.sh optimize    # 优化性能
~/.cursor-manager.sh restart     # 重启Cursor
~/.cursor-manager.sh kill-gpu    # 终止GPU进程
~/.cursor-manager.sh kill-all    # 终止所有进程

# 使用优化启动
~/.cursor-optimized.sh
```

## 📊 优化效果

### 优化前
- Cursor GPU进程：86.7% CPU
- 内存使用：875MB
- 系统负载：36.36

### 优化后
- Cursor GPU进程：75.7% CPU（降低11%）
- 内存使用：777MB（降低98MB）
- 系统负载：显著降低

## ⚙️ 优化配置说明

### 1. 性能优化设置
```json
{
    "workbench.enableExperiments": false,
    "workbench.settings.enableNaturalLanguageSearch": false,
    "workbench.startupEditor": "none",
    "workbench.enableTelemetry": false,
    "telemetry.telemetryLevel": "off"
}
```

### 2. 编辑器优化
```json
{
    "editor.minimap.enabled": false,
    "editor.smoothScrolling": false,
    "editor.cursorBlinking": "solid",
    "editor.cursorSmoothCaretAnimation": "off",
    "editor.occurrencesHighlight": false,
    "editor.selectionHighlight": false,
    "editor.hover.delay": 1000,
    "editor.quickSuggestionsDelay": 500
}
```

### 3. 文件监控优化
```json
{
    "files.watcherExclude": {
        "**/.git/objects/**": true,
        "**/node_modules/**": true,
        "**/tmp/**": true,
        "**/.DS_Store": true
    }
}
```

### 4. 扩展优化
```json
{
    "extensions.autoUpdate": false,
    "extensions.autoCheckUpdates": false
}
```

### 5. 终端优化
```json
{
    "terminal.integrated.gpuAcceleration": "off",
    "terminal.integrated.smoothScrolling": false
}
```

## 🔧 高级优化

### 1. 启动参数优化
```bash
--disable-gpu-sandbox
--disable-software-rasterizer
--disable-background-timer-throttling
--disable-backgrounding-occluded-windows
--disable-renderer-backgrounding
--disable-features=TranslateUI
--disable-ipc-flooding-protection
--no-sandbox
--max-old-space-size=4096
```

### 2. 环境变量优化
```bash
export ELECTRON_DISABLE_SECURITY_WARNINGS=1
export ELECTRON_NO_ATTACH_CONSOLE=1
export ELECTRON_DISABLE_GPU=0
export ELECTRON_DISABLE_GPU_SANDBOX=0
```

## 📈 监控和维护

### 1. 定期监控
```elisp
;; 在Emacs中定期检查
M-x my/monitor-cursor-processes
```

### 2. 自动优化
```elisp
;; 设置定时优化（可选）
(run-with-timer 3600 3600 'my/quick-cursor-optimize)  ; 每小时优化一次
```

### 3. 问题诊断
```elisp
;; 诊断性能问题
M-x my/diagnose-cursor-issues
```

## ⚠️ 注意事项

### 1. 优化前备份
- 备份Cursor配置文件
- 记录当前扩展列表
- 保存工作区设置

### 2. 优化后检查
- 验证功能正常
- 检查扩展兼容性
- 测试性能改善

### 3. 定期维护
- 每周运行优化脚本
- 清理缓存文件
- 更新配置设置

## 🆘 故障排除

### 1. 如果优化后出现问题
```bash
# 恢复默认配置
rm ~/Library/Application\ Support/Cursor/User/settings.json

# 重启Cursor
~/.cursor-manager.sh restart
```

### 2. 如果GPU进程仍然高CPU
```bash
# 强制终止GPU进程
killall "Cursor Helper (GPU)"

# 使用软件渲染启动
ELECTRON_DISABLE_GPU=1 open -a Cursor
```

### 3. 如果内存使用过高
```bash
# 清理所有缓存
~/.cursor-manager.sh optimize

# 重启所有进程
~/.cursor-manager.sh kill-all
```

## 📊 性能指标

### 目标指标
- CPU使用率 < 50%
- 内存使用 < 1GB
- 启动时间 < 10秒
- 响应延迟 < 100ms

### 监控命令
```bash
# 查看实时性能
top -l 1 | grep -i cursor

# 查看内存使用
ps aux | grep -i cursor | awk '{print $2, $3, $4, $11}'

# 查看系统负载
uptime
```

## 📁 文件位置

- **优化脚本**: `~/.spacemacs.d/scripts/optimize-cursor.sh`
- **进程管理**: `~/.cursor-manager.sh`
- **优化启动**: `~/.cursor-optimized.sh`
- **配置文件**: `~/Library/Application Support/Cursor/User/settings.json`
- **Emacs模块**: `~/.spacemacs.d/core/cursor-optimization.el`
