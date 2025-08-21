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

### 启动速度优化
- **启动时间优化**: 禁用非必要功能，延迟加载非关键模块
- **GC 优化**: 启动时优化垃圾回收，启动完成后恢复正常设置
- **字体缓存优化**: 避免字体缓存压缩，减少语法高亮装饰
- **包管理优化**: 启动时不自动加载包，禁用快速启动
- **延迟加载**: 启动后 2 秒延迟加载语言支持和 UI 增强模块

### 编程语言支持
- **Python**: LSP、虚拟环境自动检测、Black 格式化
- **Go**: goimports、golangci-lint、测试集成
- **Java**: LSP、Gradle 支持
- **JavaScript/TypeScript**: 现代前端开发
- **C/C++**: Clangd LSP 后端
- **Rust**: LSP 支持、自动格式化

### AI 集成
- **Aider**: AI 代码助手，支持多种 AI 模型
- **DeepSeek**: 完整的 DeepSeek AI 集成，包括代码审查、优化、测试生成
- **Ellama**: 本地 LLM 支持 (Ollama)
- **智能任务路由**: 自动选择最适合的 AI 模型

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

### DeepSeek 集成
- `SPC o a d` - DeepSeek 代码审查
- `SPC o a o` - DeepSeek 代码优化
- `SPC o a g` - DeepSeek 测试生成
- `SPC o a t` - DeepSeek 文档生成
- `SPC o a s` - 切换 AI 提供商
- `SPC o a r` - 智能任务路由
- `SPC o a m` - 快速访问菜单

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
- DeepSeek: 需要设置 `DEEPSEEK_API_KEY` 环境变量
- Ellama: 需要运行本地 Ollama 服务

### 启动性能测试
```bash
# 快速性能检查
emacs --batch -l scripts/startup-benchmark.el -f benchmark/run-quick-test

# 生成性能报告
emacs --batch -l scripts/startup-benchmark.el -f benchmark/generate-report
```

### 性能优化建议
- 启动时间目标：< 3 秒
- 定期运行健康检查脚本
- 监控内存使用情况
- 清理不需要的包和配置

## 📚 相关文档

- [DeepSeek 配置指南](DEEPSEEK_SETUP.md) - 详细的 DeepSeek AI 集成说明
- [性能优化指南](modules/core-performance.el) - 启动速度和性能优化配置
- [AI 集成文档](private/aider/) - Aider 和 DeepSeek 功能说明

---

*此配置经过生产环境验证，适合专业开发者使用。*
