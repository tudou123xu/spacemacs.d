#!/bin/bash
# scripts/install-emacs-mac.sh
# Author: Lin Shen (Architecture Advisor)
# Description: Automated installation of Emacs Plus with Native Comp support for macOS

set -e

echo "🚀 开始部署 Emacs Plus (Native Comp)..."

# 1. 检查 Homebrew
if ! command -v brew &> /dev/null; then
    echo "❌ 未检测到 Homebrew，请先安装 Homebrew: https://brew.sh/"
    exit 1
fi

# 2. 添加 Emacs Plus 仓库
echo "📦 添加 d12frosted/emacs-plus 仓库..."
brew tap d12frosted/emacs-plus

# 3. 安装 Emacs Plus 29 (支持 Native Comp)
# --with-native-comp: 开启原生编译（配合我们的 early-init.el 提升 3-5 倍性能）
# --with-spacemacs-icon: 使用 Spacemacs 风格图标
echo "⬇️ 正在安装 Emacs Plus 29..."
brew install emacs-plus@29 --with-native-comp --with-spacemacs-icon

# 4. 链接到应用程序目录
echo "🔗 链接 Emacs.app 到 /Applications..."
if [ -d "/Applications/Emacs.app" ]; then
    echo "⚠️ /Applications/Emacs.app 已存在，正在备份..."
    mv /Applications/Emacs.app /Applications/Emacs.app.bak
fi
ln -s /opt/homebrew/opt/emacs-plus@29/Emacs.app /Applications/Emacs.app

# 5. 验证配置链接
echo "🔍 验证配置文件链接..."
if [ -d "$HOME/.emacs.d" ] && [ -d "$HOME/.spacemacs.d" ]; then
    echo "✅ 配置环境正常：Spacemacs 核心已就绪，用户配置已就绪。"
else
    echo "⚠️ 警告：未检测到标准的 Spacemacs 目录结构，请确保 ~/.emacs.d 是 Spacemacs 仓库。"
fi

echo "🎉 安装完成！"
echo "👉 你现在可以在 Launchpad 或 Spotlight 中启动 'Emacs' 了。"
echo "💡 首次启动时，Native Comp 会在后台编译核心库，CPU 可能会短暂升高，这是正常的。"
