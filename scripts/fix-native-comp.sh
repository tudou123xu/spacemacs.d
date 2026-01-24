#!/bin/bash
# 修复 Emacs 原生编译临时目录问题
# Author: xuzhifeng

echo "=== Emacs 原生编译修复工具 ==="
echo ""

EMACS_DIR="${HOME}/.emacs.d"
SPACEMACS_DIR="${HOME}/.spacemacs.d"

# 检查 Emacs 目录
if [ ! -d "$EMACS_DIR" ]; then
    echo "✗ Emacs 目录不存在: $EMACS_DIR"
    exit 1
fi

echo "✓ Emacs 目录: $EMACS_DIR"

# 创建 eln-cache 目录
ELN_CACHE_DIR="${EMACS_DIR}/eln-cache"
if [ ! -d "$ELN_CACHE_DIR" ]; then
    mkdir -p "$ELN_CACHE_DIR"
    echo "✓ 创建 eln-cache 目录: $ELN_CACHE_DIR"
else
    echo "✓ eln-cache 目录已存在: $ELN_CACHE_DIR"
fi

# 创建临时目录
TEMP_DIR="${EMACS_DIR}/tmp"
if [ ! -d "$TEMP_DIR" ]; then
    mkdir -p "$TEMP_DIR"
    echo "✓ 创建临时目录: $TEMP_DIR"
else
    echo "✓ 临时目录已存在: $TEMP_DIR"
fi

# 设置正确的权限
chmod 755 "$ELN_CACHE_DIR" 2>/dev/null
chmod 755 "$TEMP_DIR" 2>/dev/null

echo ""
echo "=== 清理可能的问题文件 ==="

# 清理损坏的 eln 文件
if [ -d "$ELN_CACHE_DIR" ]; then
    BROKEN_FILES=$(find "$ELN_CACHE_DIR" -name "*.eln" -size 0 2>/dev/null | wc -l)
    if [ "$BROKEN_FILES" -gt 0 ]; then
        echo "发现 $BROKEN_FILES 个损坏的 .eln 文件，正在清理..."
        find "$ELN_CACHE_DIR" -name "*.eln" -size 0 -delete 2>/dev/null
        echo "✓ 已清理损坏的文件"
    else
        echo "✓ 没有发现损坏的 .eln 文件"
    fi
fi

# 清理临时目录中的旧文件
if [ -d "$TEMP_DIR" ]; then
    OLD_FILES=$(find "$TEMP_DIR" -type f -mtime +7 2>/dev/null | wc -l)
    if [ "$OLD_FILES" -gt 0 ]; then
        echo "发现 $OLD_FILES 个超过7天的临时文件，正在清理..."
        find "$TEMP_DIR" -type f -mtime +7 -delete 2>/dev/null
        echo "✓ 已清理旧的临时文件"
    else
        echo "✓ 没有需要清理的旧临时文件"
    fi
fi

echo ""
echo "=== 修复完成 ==="
echo ""
echo "建议操作:"
echo "1. 重启 Emacs 以应用修复"
echo "2. 如果问题仍然存在，在 Emacs 中运行:"
echo "   M-x my/fix-native-compilation"
echo "3. 如果仍有问题，可以完全禁用原生编译:"
echo "   M-x my/disable-native-compilation"
echo ""
echo "日志位置: ${SPACEMACS_DIR}/logs/"
echo ""

