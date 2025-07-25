#!/bin/bash
# setup.sh - Spacemacs 模块化配置部署脚本

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 打印彩色消息
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# 检查是否为 macOS
is_macos() {
    [[ "$OSTYPE" == "darwin"* ]]
}

# 检查是否为 Linux
is_linux() {
    [[ "$OSTYPE" == "linux-gnu"* ]]
}

# 检查必要工具
check_prerequisites() {
    print_status "检查前置条件..."
    
    # 检查 Emacs
    if ! command -v emacs &> /dev/null; then
        print_error "Emacs 未安装，请先安装 Emacs"
        exit 1
    fi
    
    # 检查 Git
    if ! command -v git &> /dev/null; then
        print_error "Git 未安装，请先安装 Git"
        exit 1
    fi
    
    print_success "前置条件检查通过"
}

# 备份现有配置
backup_existing_config() {
    if [[ -d "$HOME/.spacemacs.d" ]]; then
        print_warning "发现现有 Spacemacs 配置"
        local backup_dir="$HOME/.spacemacs.d.backup.$(date +%Y%m%d_%H%M%S)"
        print_status "备份现有配置到: $backup_dir"
        mv "$HOME/.spacemacs.d" "$backup_dir"
        print_success "配置已备份"
    fi
    
    if [[ -f "$HOME/.spacemacs" ]]; then
        print_warning "发现现有 .spacemacs 文件"
        local backup_file="$HOME/.spacemacs.backup.$(date +%Y%m%d_%H%M%S)"
        print_status "备份现有配置到: $backup_file"
        mv "$HOME/.spacemacs" "$backup_file"
        print_success ".spacemacs 文件已备份"
    fi
}

# 创建目录结构
create_directories() {
    print_status "创建目录结构..."
    
    local dirs=(
        "$HOME/.spacemacs.d/modules"
        "$HOME/.spacemacs.d/private/aider"
        "$HOME/.spacemacs.d/private/ellama"
        "$HOME/.spacemacs.d/private/db-layer"
        "$HOME/.spacemacs.d/scripts"
        "$HOME/.spacemacs.d/logs"
    )
    
    for dir in "${dirs[@]}"; do
        mkdir -p "$dir"
        print_success "创建目录: $dir"
    done
}

# 设置文件权限
set_permissions() {
    print_status "设置文件权限..."
    
    # 设置日志目录权限
    chmod 700 "$HOME/.spacemacs.d/logs"
    
    # 设置脚本执行权限
    if [[ -f "$HOME/.spacemacs.d/scripts/setup.sh" ]]; then
        chmod +x "$HOME/.spacemacs.d/scripts/setup.sh"
    fi
    
    print_success "文件权限设置完成"
}

# 安装可选依赖
install_optional_dependencies() {
    print_status "检查可选依赖..."
    
    # 检查 ripgrep
    if ! command -v rg &> /dev/null; then
        print_warning "推荐安装 ripgrep 以获得更好的搜索性能"
        if is_macos && command -v brew &> /dev/null; then
            print_status "使用 Homebrew 安装 ripgrep..."
            brew install ripgrep
        elif is_linux && command -v apt &> /dev/null; then
            print_status "使用 apt 安装 ripgrep..."
            sudo apt update && sudo apt install -y ripgrep
        fi
    fi
    
    # 检查 fd
    if ! command -v fd &> /dev/null; then
        print_warning "推荐安装 fd 以获得更好的文件查找性能"
        if is_macos && command -v brew &> /dev/null; then
            print_status "使用 Homebrew 安装 fd..."
            brew install fd
        elif is_linux && command -v apt &> /dev/null; then
            print_status "使用 apt 安装 fd..."
            sudo apt update && sudo apt install -y fd-find
        fi
    fi
}

# 验证配置
validate_installation() {
    print_status "验证安装..."
    
    # 检查关键文件
    local required_files=(
        "$HOME/.spacemacs.d/init.el"
        "$HOME/.spacemacs.d/user-config.el"
        "$HOME/.spacemacs.d/modules/core-performance.el"
        "$HOME/.spacemacs.d/private/aider/packages.el"
    )
    
    for file in "${required_files[@]}"; do
        if [[ ! -f "$file" ]]; then
            print_error "缺少关键文件: $file"
            return 1
        fi
    done
    
    print_success "配置验证通过"
}

# 显示后续步骤
show_next_steps() {
    echo
    print_success "🎉 Spacemacs 模块化配置安装完成！"
    echo
    print_status "后续步骤："
    echo "1. 启动 Emacs"
    echo "2. 等待包管理器下载和安装必要的包"
    echo "3. 享受优化后的 Spacemacs 体验！"
    echo
    print_status "可选配置："
    echo "• AI 功能: 安装 aider-chat 和配置 Ollama"
    echo "• 字体优化: 安装 SF Mono 或 Source Code Pro"
    echo "• 开发工具: 配置相应语言的 LSP 服务器"
    echo
    print_status "使用验证脚本检查配置:"
    echo "emacs --batch -l ~/.spacemacs.d/scripts/validate-config.el -f validate/run-all-checks"
}

# 主函数
main() {
    echo "======================================"
    echo "  Spacemacs 模块化配置部署脚本"
    echo "======================================"
    echo
    
    check_prerequisites
    backup_existing_config
    create_directories
    set_permissions
    install_optional_dependencies
    
    if validate_installation; then
        show_next_steps
    else
        print_error "安装验证失败，请检查配置"
        exit 1
    fi
}

# 运行主函数
main "$@" 