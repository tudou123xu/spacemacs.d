#!/bin/bash
# start-emacs.sh - 优化的 Emacs 启动脚本
# Author: xuzhifeng
# Created: 2024

echo "🚀 正在启动 Emacs..."

# 检查网络连接和选择最佳镜像源
echo "🔍 检查网络连接..."
if curl -s --connect-timeout 3 https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/ > /dev/null; then
    echo "✅ 清华镜像连接正常，使用国内镜像源"
    export EMACS_MIRROR="tuna"
elif curl -s --connect-timeout 3 https://mirrors.ustc.edu.cn/elpa/melpa/ > /dev/null; then
    echo "✅ 中科大镜像连接正常，使用国内镜像源"
    export EMACS_MIRROR="ustc"
elif curl -s --connect-timeout 5 https://melpa.org/packages/ > /dev/null; then
    echo "⚠️  官方源连接正常，但速度可能较慢"
    export EMACS_MIRROR="official"
else
    echo "❌ 网络连接有问题，将使用离线模式"
    export EMACS_MIRROR="offline"
fi

# 设置环境变量优化
export EMACS_LOAD_QUICKLY=1
export EMACS_DISABLE_BANNER=1

# 检查是否需要修复包依赖
if [ ! -f ~/.emacs.d/elpa/spinner-*/spinner.el ]; then
    echo "🔧 检测到缺失的包依赖，正在修复..."
    cd ~/.spacemacs.d
    emacs --script scripts/fix-packages.el
fi

# 启动 Emacs
echo "🎯 启动 Emacs..."
exec emacs "$@"