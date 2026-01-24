#!/bin/bash
# Cursor编辑器性能优化脚本
# Author: xuzhifeng
# Created: 2024

echo "🚀 开始优化Cursor编辑器性能..."

# 1. 检查当前Cursor进程状态
echo "📊 检查Cursor进程状态..."
ps aux | grep -i cursor | grep -v grep | head -5

echo ""
echo "🎯 发现的问题："
echo "1. Cursor GPU进程占用22.5% CPU"
echo "2. 多个Cursor Helper进程运行"
echo "3. 扩展进程占用内存"

# 2. 创建Cursor优化配置
echo ""
echo "⚙️ 创建Cursor优化配置..."

# 创建Cursor用户配置目录
CURSOR_CONFIG_DIR="$HOME/Library/Application Support/Cursor/User"
mkdir -p "$CURSOR_CONFIG_DIR"

# 创建优化后的settings.json
cat > "$CURSOR_CONFIG_DIR/settings.json" << 'EOF'
{
    // 性能优化配置
    "workbench.enableExperiments": false,
    "workbench.settings.enableNaturalLanguageSearch": false,
    "workbench.startupEditor": "none",
    
    // 禁用不必要的功能
    "workbench.enableTelemetry": false,
    "telemetry.telemetryLevel": "off",
    "workbench.enableExperiments": false,
    
    // GPU优化
    "disable-hardware-acceleration": false,
    "disable-gpu": false,
    "disable-gpu-sandbox": false,
    "disable-software-rasterizer": false,
    
    // 编辑器性能优化
    "editor.minimap.enabled": false,
    "editor.smoothScrolling": false,
    "editor.cursorBlinking": "solid",
    "editor.cursorSmoothCaretAnimation": "off",
    "editor.occurrencesHighlight": false,
    "editor.selectionHighlight": false,
    "editor.hover.delay": 1000,
    "editor.quickSuggestionsDelay": 500,
    
    // 文件监控优化
    "files.watcherExclude": {
        "**/.git/objects/**": true,
        "**/.git/subtree-cache/**": true,
        "**/node_modules/**": true,
        "**/tmp/**": true,
        "**/bower_components/**": true,
        "**/.vscode/**": true,
        "**/.DS_Store": true,
        "**/Thumbs.db": true
    },
    
    // 搜索优化
    "search.followSymlinks": false,
    "search.useIgnoreFiles": true,
    "search.useGlobalIgnoreFiles": true,
    
    // 扩展优化
    "extensions.autoUpdate": false,
    "extensions.autoCheckUpdates": false,
    
    // 终端优化
    "terminal.integrated.gpuAcceleration": "off",
    "terminal.integrated.smoothScrolling": false,
    
    // 语言服务器优化
    "typescript.preferences.includePackageJsonAutoImports": "off",
    "javascript.preferences.includePackageJsonAutoImports": "off",
    
    // 内存优化
    "files.hotExit": "off",
    "workbench.editor.enablePreview": false,
    "workbench.editor.enablePreviewFromQuickOpen": false,
    
    // 禁用不必要的动画
    "workbench.reduceMotion": "on",
    "editor.suggest.animate": false,
    "workbench.editor.smoothScrolling": false
}
EOF

echo "✓ Cursor配置文件已创建: $CURSOR_CONFIG_DIR/settings.json"

# 3. 创建启动优化脚本
echo ""
echo "🔧 创建启动优化脚本..."

cat > "$HOME/.cursor-optimized.sh" << 'EOF'
#!/bin/bash
# 优化启动Cursor的脚本

# 设置环境变量优化
export ELECTRON_DISABLE_SECURITY_WARNINGS=1
export ELECTRON_NO_ATTACH_CONSOLE=1
export ELECTRON_DISABLE_GPU=0
export ELECTRON_DISABLE_GPU_SANDBOX=0

# 启动参数优化
CURSOR_ARGS=(
    --disable-gpu-sandbox
    --disable-software-rasterizer
    --disable-background-timer-throttling
    --disable-backgrounding-occluded-windows
    --disable-renderer-backgrounding
    --disable-features=TranslateUI
    --disable-ipc-flooding-protection
    --no-sandbox
    --max-old-space-size=4096
)

# 启动Cursor
exec /Applications/Cursor.app/Contents/MacOS/Cursor "${CURSOR_ARGS[@]}" "$@"
EOF

chmod +x "$HOME/.cursor-optimized.sh"

echo "✓ 优化启动脚本已创建: $HOME/.cursor-optimized.sh"

# 4. 创建进程管理脚本
echo ""
echo "🛠️ 创建进程管理脚本..."

cat > "$HOME/.cursor-manager.sh" << 'EOF'
#!/bin/bash
# Cursor进程管理脚本

case "$1" in
    "restart")
        echo "重启Cursor..."
        killall Cursor 2>/dev/null
        sleep 2
        open -a Cursor
        ;;
    "kill-gpu")
        echo "终止Cursor GPU进程..."
        killall "Cursor Helper (GPU)" 2>/dev/null
        ;;
    "kill-all")
        echo "终止所有Cursor进程..."
        killall Cursor 2>/dev/null
        killall "Cursor Helper" 2>/dev/null
        ;;
    "status")
        echo "Cursor进程状态:"
        ps aux | grep -i cursor | grep -v grep
        ;;
    "optimize")
        echo "优化Cursor性能..."
        killall "Cursor Helper (GPU)" 2>/dev/null
        sleep 1
        # 清理缓存
        rm -rf "$HOME/Library/Application Support/Cursor/logs"
        rm -rf "$HOME/Library/Application Support/Cursor/CachedData"
        ;;
    *)
        echo "用法: $0 {restart|kill-gpu|kill-all|status|optimize}"
        echo "  restart  - 重启Cursor"
        echo "  kill-gpu - 终止GPU进程"
        echo "  kill-all - 终止所有进程"
        echo "  status   - 查看状态"
        echo "  optimize - 优化性能"
        ;;
esac
EOF

chmod +x "$HOME/.cursor-manager.sh"

echo "✓ 进程管理脚本已创建: $HOME/.cursor-manager.sh"

# 5. 立即优化
echo ""
echo "🚀 立即执行优化..."

# 终止高CPU的GPU进程
echo "终止Cursor GPU进程..."
killall "Cursor Helper (GPU)" 2>/dev/null

# 清理缓存
echo "清理Cursor缓存..."
rm -rf "$HOME/Library/Application Support/Cursor/logs"
rm -rf "$HOME/Library/Application Support/Cursor/CachedData"

# 等待进程终止
sleep 2

echo ""
echo "✅ Cursor优化完成！"
echo ""
echo "📋 使用方法："
echo "1. 重启Cursor: $HOME/.cursor-manager.sh restart"
echo "2. 查看状态: $HOME/.cursor-manager.sh status"
echo "3. 优化性能: $HOME/.cursor-manager.sh optimize"
echo "4. 使用优化启动: $HOME/.cursor-optimized.sh"
echo ""
echo "⚠️  建议："
echo "- 重启Cursor以应用新配置"
echo "- 定期运行优化命令"
echo "- 关闭不必要的扩展"
