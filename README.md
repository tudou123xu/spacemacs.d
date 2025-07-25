# Spacemacs 模块化配置

这是一个高度优化的 Spacemacs 配置，采用模块化设计，支持多语言开发和 AI 集成。

## 📁 配置结构

```
.spacemacs.d/
├── init.el              # 主配置文件
├── user-config.el       # 用户配置入口
├── .spacemacs.env       # 环境变量配置
├── modules/             # 功能模块目录
│   ├── core-performance.el    # 核心性能优化
│   ├── lang-support.el        # 编程语言支持
│   ├── ui-enhancement.el      # 界面增强
│   ├── system-integration.el  # 系统集成
│   ├── security-audit.el      # 安全审计
│   ├── macos-specific.el      # macOS 专属配置
│   └── windows-specific.el    # Windows 专属配置
└── private/             # 私有层目录
    ├── aider/           # AI 代码助手层
    ├── ellama/          # LLM 集成层
    └── db-layer/        # 数据库连接层
```

## 🚀 主要特性

### 编程语言支持
- **Python**: LSP、虚拟环境自动检测、Black 格式化
- **Go**: goimports、golangci-lint、测试集成
- **Java**: LSP、Gradle 支持
- **JavaScript/TypeScript**: 现代前端开发
- **C/C++**: Clangd LSP 后端
- **Rust**: LSP 支持、自动格式化

### AI 集成
- **Aider**: GPT-4 代码助手集成
- **Ellama**: 本地 LLM 支持 (Ollama)

### 性能优化
- 动态 GC 调优
- LSP 性能优化
- 跨平台字体智能选择
- 模块化按需加载

### 安全特性
- 配置变更审计日志
- 敏感信息遮蔽
- 文件权限保护

## 🔧 快捷键

### Aider 集成
- `SPC o a s` - 启动 Aider 会话
- `SPC o a a` - 添加当前文件
- `SPC o a r` - 审查当前函数
- `SPC o a e` - 解释选中代码

### Ellama 集成
- `SPC o e l` - 新建 LLM 聊天
- `SPC o e r` - 重写选中区域

## ⚡ 安装使用

1. 备份现有配置：
```bash
mv ~/.spacemacs.d ~/.spacemacs.d.bak
```

2. 克隆此配置：
```bash
git clone <repo-url> ~/.spacemacs.d
```

3. 启动 Spacemacs 并等待包安装完成

## 🔧 自定义配置

所有配置模块都在 `modules/` 目录下，可以根据需要启用或禁用特定功能。

### 平台专属配置
- macOS 用户：自动加载 `macos-specific.el`
- Windows 用户：自动加载 `windows-specific.el`

### AI 功能配置
- Aider: 需要安装 `aider-chat` 命令行工具
- Ellama: 需要运行本地 Ollama 服务

---

*此配置经过生产环境验证，适合专业开发者使用。*
