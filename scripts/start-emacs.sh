#!/bin/bash
# 启动 Emacs 并测试配置

echo "=== Spacemacs 配置测试 ==="
echo "开始启动 Emacs 并测试配置..."

# 设置环境变量
export EMACS_DEBUG_INIT=1

# 启动 Emacs 并测试配置
emacs --debug-init --eval "
(progn
  (message \"开始测试 Spacemacs 配置...\")
  (condition-case err
      (progn
        (load-file \"~/.spacemacs.d/init.el\")
        (message \"✓ 配置加载成功\"))
    (error
     (message \"✗ 配置加载失败: %s\" (error-message-string err))
     (kill-emacs 1)))
  (message \"配置测试完成\"))
" --batch

if [ $? -eq 0 ]; then
    echo "✓ 配置测试通过"
else
    echo "✗ 配置测试失败"
    exit 1
fi

echo "=== 测试完成 ==="