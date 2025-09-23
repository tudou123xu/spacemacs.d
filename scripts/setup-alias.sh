#!/bin/bash
# 设置 Emacs 别名

echo "=== 设置 Emacs 别名 ==="

# 创建别名文件
cat > "$HOME/.emacs-alias" << 'EOF'
# Emacs 安全启动别名
alias emacs-safe="$HOME/.spacemacs.d/scripts/emacs-no-native"
alias emacs="$HOME/.spacemacs.d/scripts/emacs-no-native"
alias emacs-original="$(which emacs)"
EOF

echo "✓ 已创建别名文件: ~/.emacs-alias"
echo ""
echo "要使用这些别名，请运行:"
echo "  source ~/.emacs-alias"
echo ""
echo "或者将以下内容添加到您的 ~/.zshrc 或 ~/.bashrc:"
echo "  source ~/.emacs-alias"
echo ""
echo "然后您就可以直接使用 'emacs' 命令安全启动 Emacs 了！"
