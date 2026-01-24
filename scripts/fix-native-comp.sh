#!/bin/bash
# scripts/fix-native-comp.sh
# Author: Lin Shen
# Description: Fix "error invoking gcc driver" for Emacs Native Comp on macOS

set -e

echo "🔧 正在修复 Native Comp 编译环境..."

# 1. 检查 Homebrew
if ! command -v brew &> /dev/null; then
    echo "❌ 未检测到 Homebrew"
    exit 1
fi

# 2. 安装 libgccjit 和 gcc
# Emacs-plus 通常依赖 gcc (目前主要是 gcc-14 或 gcc-13)
echo "📦 安装/更新 libgccjit 和 gcc..."
brew install libgccjit gcc

# 3. 查找 GCC 版本
GCC_BIN=""
for ver in 14 13 12; do
    if [ -f "/opt/homebrew/bin/gcc-$ver" ]; then
        GCC_BIN="/opt/homebrew/bin/gcc-$ver"
        echo "✅ 找到 GCC: $GCC_BIN"
        break
    fi
done

if [ -z "$GCC_BIN" ]; then
    echo "❌ 未找到 Homebrew 安装的 GCC (gcc-14/13/12)"
    echo "请尝试手动运行: brew install gcc"
    exit 1
fi

echo "🎉 依赖安装完成。"
echo "接下来，early-init.el 将会自动配置 Emacs 指向这个编译器。"
