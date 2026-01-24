#!/bin/bash
# scripts/fix-vterm-build.sh
# Author: Lin Shen
# Description: Fix vterm build error "ar: temporary file: No such file or directory" and locale issues

set -e

echo "🔧 正在修复 vterm 编译环境..."

# 1. 修复 Locale 设置 (解决 perl warning)
echo "🌍 修复 Locale 设置..."
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

# 2. 检查并修复 libtool/ar 问题
# macOS 的 ar 命令有时会因为环境变量问题报错，确保使用系统自带的工具链
echo "🛠️ 配置构建工具链..."

# 强制使用系统 ar
if [ -f "/usr/bin/ar" ]; then
    export AR="/usr/bin/ar"
    echo "✅ 设置 AR=/usr/bin/ar"
else
    echo "⚠️ 未找到 /usr/bin/ar，尝试使用 path 中的 ar"
fi

# 3. 清理旧的 vterm 构建缓存
VTERM_BUILD_DIR="$HOME/.emacs.d/elpa/30.2/develop/vterm-20251119.1653/build"
if [ -d "$VTERM_BUILD_DIR" ]; then
    echo "🧹 清理旧的构建目录: $VTERM_BUILD_DIR"
    rm -rf "$VTERM_BUILD_DIR"
fi

# 4. 提示用户手动重试
echo "🎉 环境修复完成。"
echo "👉 请重启 Emacs，vterm 应该能自动重新编译成功。"
echo "如果仍然失败，请尝试在 Emacs 中运行: M-x vterm-module-compile"
