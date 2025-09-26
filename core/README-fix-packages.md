# 包修复工具使用说明

## 🚀 快速开始

### 方法一：自动加载（推荐）

由于 `fix-missing-packages.el` 已经集成到 `core` 层，它会自动加载。直接使用：

```elisp
M-x my/fix-missing-packages
```

或者快速修复：

```elisp
M-x my/quick-fix-packages
```

### 方法二：手动加载

如果需要手动加载：

```elisp
(load-file "~/.spacemacs.d/core/fix-missing-packages.el")
M-x my/fix-missing-packages
```

### 方法三：检查包状态

在修复之前，可以先检查哪些包缺失：

```elisp
M-x my/check-package-status
```

## 📋 修复的包列表

### 关键依赖包
- `spinner` (1.7.2) - 加载动画包
- `auctex` (11.87) - LaTeX 编辑支持
- `mathjax` (0.1) - 数学公式渲染

### LSP 相关包
- `lsp-mode` - 核心 LSP 功能
- `lsp-ui` - LSP 用户界面
- `lsp-treemacs` - LSP 树形视图
- `lsp-pyright` - Python LSP 支持
- `lsp-origami` - LSP 代码折叠
- `lsp-latex` - LaTeX LSP 支持
- `lsp-java` - Java LSP 支持
- `lsp-ivy` - LSP 搜索集成
- `dap-mode` - 调试适配器协议

### LaTeX 相关包
- `evil-tex` - LaTeX 编辑支持
- `company-auctex` - LaTeX 自动补全

## 🔧 故障排除

### 如果修复失败

1. **检查网络连接**：
   ```elisp
   M-x my/check-package-status
   ```

2. **手动刷新包列表**：
   ```elisp
   M-x package-refresh-contents
   ```

3. **尝试不同的包源**：
   脚本会自动尝试多个包源（清华镜像、官方源等）

### 如果某些包仍然无法安装

可以尝试手动安装：

```elisp
M-x package-install
# 然后输入包名，如：spinner
```

## 📝 使用示例

### 完整修复流程

```elisp
;; 1. 检查当前状态
M-x my/check-package-status

;; 2. 运行修复
M-x my/fix-missing-packages

;; 3. 重启 Emacs
M-x restart-emacs
```

### 快速修复流程

```elisp
;; 1. 快速修复
M-x my/quick-fix-packages

;; 2. 重启 Emacs
M-x restart-emacs
```

## ⚠️ 注意事项

1. **重启 Emacs**：修复完成后必须重启 Emacs 才能生效
2. **网络要求**：需要稳定的网络连接来下载包
3. **权限问题**：确保有写入 `~/.emacs.d/elpa/` 目录的权限
4. **版本兼容**：脚本会自动处理版本兼容性问题

## 🆘 获取帮助

如果遇到问题，可以：

1. 查看 Emacs 的 `*Messages*` 缓冲区了解详细错误信息
2. 运行 `M-x my/check-package-status` 查看包安装状态
3. 检查网络连接和包源配置

## 📊 修复效果

修复成功后，以下功能将恢复正常：

- ✅ LSP 语言服务器支持（所有编程语言）
- ✅ 调试功能（DAP 调试器）
- ✅ LaTeX 文档编辑
- ✅ 包管理界面
- ✅ 开发文档查看

## 📁 文件位置

- **主文件**：`~/.spacemacs.d/core/fix-missing-packages.el`
- **配置**：通过 `config-manager.el` 自动加载
- **优先级**：在 `core` 层中，优先级为 2
