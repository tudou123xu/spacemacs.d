# Emacs 原生编译问题修复说明

## 问题描述
Emacs 在启动时遇到原生编译错误，主要问题包括：
- Xcode 许可证未接受
- GCC 驱动错误
- 语言环境设置问题 (en_CN.UTF-8 vs en_US.UTF-8)

## 解决方案

### 1. 完全禁用原生编译
创建了 `early-init.el` 文件，在 Emacs 启动的最早期就禁用所有原生编译功能。

### 2. 创建安全启动器
- `scripts/emacs-no-native` - 完全禁用原生编译的启动器
- `scripts/start-emacs-safe.sh` - 安全启动脚本（使用上述启动器）

### 3. 环境变量修复
- 设置 `LANG=en_US.UTF-8`
- 设置 `LC_ALL=en_US.UTF-8`
- 清理所有原生编译缓存

## 使用方法

### 方法一：直接使用启动器
```bash
# 使用完全禁用原生编译的启动器
~/.spacemacs.d/scripts/emacs-no-native

# 或使用安全启动脚本
~/.spacemacs.d/scripts/start-emacs-safe.sh
```

### 方法二：设置别名（推荐）
```bash
# 设置别名
source ~/.emacs-alias

# 然后直接使用
emacs
```

### 方法三：永久设置别名
```bash
# 将以下内容添加到 ~/.zshrc 或 ~/.bashrc
echo "source ~/.emacs-alias" >> ~/.zshrc
source ~/.zshrc

# 然后直接使用
emacs
```

## 测试验证

运行测试脚本验证修复：
```bash
~/.spacemacs.d/scripts/test-complete-fix.sh
```

## 文件说明

- `early-init.el` - 早期初始化配置，禁用原生编译
- `scripts/emacs-no-native` - 完全禁用原生编译的启动器
- `scripts/start-emacs-safe.sh` - 安全启动脚本
- `scripts/setup-alias.sh` - 别名设置脚本
- `scripts/test-complete-fix.sh` - 测试脚本
- `~/.emacs-alias` - 别名配置文件

## 注意事项

1. 如果将来需要启用原生编译，需要先解决 Xcode 许可证问题
2. 所有启动器都会自动清理原生编译缓存
3. 语言环境已设置为 `en_US.UTF-8`，避免中文本地化问题

## 故障排除

如果仍然遇到问题：
1. 确保所有脚本都有执行权限：`chmod +x ~/.spacemacs.d/scripts/*`
2. 检查 Xcode 命令行工具：`xcode-select -p`
3. 接受 Xcode 许可证：`sudo xcodebuild -license accept`
4. 运行测试脚本验证修复

## 恢复原始 Emacs

如果需要使用原始 Emacs（启用原生编译）：
```bash
# 使用原始 Emacs
emacs-original

# 或直接调用
$(which emacs)
```
