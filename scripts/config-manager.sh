#!/bin/bash
# config-manager.sh - Spacemacs 配置管理脚本
# Author: xuzhifeng
# Created: 2024

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

# 配置目录
CONFIG_DIR="$HOME/.spacemacs.d"
MODULES_DIR="$CONFIG_DIR/modules"
SCRIPTS_DIR="$CONFIG_DIR/scripts"
LOGS_DIR="$CONFIG_DIR/logs"

# 打印函数
print_header() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}  Spacemacs 配置管理器${NC}"
    echo -e "${BLUE}========================================${NC}"
}

print_info() {
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

print_step() {
    echo -e "${PURPLE}[STEP]${NC} $1"
}

# 检查前置条件
check_prerequisites() {
    print_step "检查前置条件..."
    
    if ! command -v emacs &> /dev/null; then
        print_error "Emacs 未安装"
        exit 1
    fi
    
    print_success "前置条件检查通过"
}

# 健康检查
health_check() {
    print_step "运行健康检查..."
    
    if [[ -f "$SCRIPTS_DIR/health-check.el" ]]; then
        emacs --batch -l "$SCRIPTS_DIR/health-check.el" 2>&1 | while read -r line; do
            if [[ $line == *"✓"* ]]; then
                echo -e "${GREEN}$line${NC}"
            elif [[ $line == *"⚠"* ]]; then
                echo -e "${YELLOW}$line${NC}"
            elif [[ $line == *"✗"* ]]; then
                echo -e "${RED}$line${NC}"
            else
                echo "$line"
            fi
        done
    else
        print_warning "健康检查脚本不存在"
    fi
}

# 修复包依赖
fix_packages() {
    print_step "修复包依赖..."
    
    if [[ -f "$SCRIPTS_DIR/fix-packages.el" ]]; then
        emacs --script "$SCRIPTS_DIR/fix-packages.el"
        print_success "包依赖修复完成"
    else
        print_error "包修复脚本不存在"
        exit 1
    fi
}

# 验证配置
validate_config() {
    print_step "验证配置..."
    
    local required_files=(
        "$CONFIG_DIR/init.el"
        "$CONFIG_DIR/user-config.el"
        "$MODULES_DIR/core-performance.el"
        "$MODULES_DIR/error-handling.el"
        "$MODULES_DIR/package-fix.el"
    )
    
    local missing_files=()
    
    for file in "${required_files[@]}"; do
        if [[ ! -f "$file" ]]; then
            missing_files+=("$file")
        fi
    done
    
    if [[ ${#missing_files[@]} -eq 0 ]]; then
        print_success "配置验证通过"
    else
        print_error "缺少关键文件:"
        for file in "${missing_files[@]}"; do
            echo "  - $file"
        done
        exit 1
    fi
}

# 清理缓存
clean_cache() {
    print_step "清理缓存..."
    
    # 清理 Emacs 缓存
    if [[ -d "$HOME/.emacs.d/elpa" ]]; then
        print_info "清理包缓存..."
        rm -rf "$HOME/.emacs.d/elpa"
    fi
    
    # 清理日志文件
    if [[ -d "$LOGS_DIR" ]]; then
        print_info "清理日志文件..."
        find "$LOGS_DIR" -name "*.log" -mtime +7 -delete
    fi
    
    print_success "缓存清理完成"
}

# 显示状态
show_status() {
    print_step "显示配置状态..."
    
    echo
    print_info "配置目录: $CONFIG_DIR"
    print_info "模块目录: $MODULES_DIR"
    print_info "脚本目录: $SCRIPTS_DIR"
    echo
    
    # 显示模块状态
    print_info "模块状态:"
    for module in core-performance error-handling package-fix system-integration ui-enhancement lang-support security-audit; do
        if [[ -f "$MODULES_DIR/$module.el" ]]; then
            echo -e "  ${GREEN}✓${NC} $module"
        else
            echo -e "  ${RED}✗${NC} $module"
        fi
    done
    
    echo
    print_info "脚本状态:"
    for script in health-check.el fix-packages.el start-emacs.sh; do
        if [[ -f "$SCRIPTS_DIR/$script" ]]; then
            echo -e "  ${GREEN}✓${NC} $script"
        else
            echo -e "  ${RED}✗${NC} $script"
        fi
    done
}

# 显示帮助
show_help() {
    echo "用法: $0 [选项]"
    echo
    echo "选项:"
    echo "  health      运行健康检查"
    echo "  fix         修复包依赖"
    echo "  validate    验证配置"
    echo "  clean       清理缓存"
    echo "  status      显示状态"
    echo "  help        显示帮助"
    echo
    echo "示例:"
    echo "  $0 health    # 运行健康检查"
    echo "  $0 fix       # 修复包依赖"
    echo "  $0 status    # 显示配置状态"
}

# 主函数
main() {
    print_header
    
    case "${1:-help}" in
        health)
            check_prerequisites
            health_check
            ;;
        fix)
            check_prerequisites
            fix_packages
            ;;
        validate)
            check_prerequisites
            validate_config
            ;;
        clean)
            clean_cache
            ;;
        status)
            show_status
            ;;
        help|--help|-h)
            show_help
            ;;
        *)
            print_error "未知选项: $1"
            show_help
            exit 1
            ;;
    esac
}

# 运行主函数
main "$@"