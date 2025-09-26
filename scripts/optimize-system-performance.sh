#!/bin/bash
# 系统性能优化脚本
# Author: xuzhifeng
# Created: 2024

echo "🔧 开始系统性能优化..."

# 1. 检查高CPU进程
echo "📊 检查高CPU进程..."
ps aux | sort -k3 -nr | head -10

echo ""
echo "🎯 发现的主要问题："
echo "1. Cursor GPU进程占用79.5% CPU"
echo "2. Chrome音频服务占用58.6% CPU"
echo "3. 系统负载过高 (Load: 36.36)"

echo ""
echo "🛠️ 优化建议："

# 2. 优化Cursor设置
echo "1. 优化Cursor编辑器："
echo "   - 关闭不必要的GPU加速"
echo "   - 减少扩展数量"
echo "   - 关闭实时预览功能"

# 3. 优化Chrome设置
echo "2. 优化Chrome浏览器："
echo "   - 关闭不必要的标签页"
echo "   - 禁用音频服务（如果不需要）"
echo "   - 清理浏览器缓存"

# 4. 系统优化
echo "3. 系统优化："
echo "   - 重启高CPU进程"
echo "   - 清理系统缓存"
echo "   - 检查后台任务"

# 5. 提供具体命令
echo ""
echo "🚀 立即执行的优化命令："

echo "# 重启Cursor（如果可能）"
echo "killall Cursor 2>/dev/null || echo 'Cursor未运行'"

echo ""
echo "# 重启Chrome音频服务"
echo "killall 'Google Chrome Helper' 2>/dev/null || echo 'Chrome Helper未运行'"

echo ""
echo "# 清理系统缓存"
echo "sudo purge"

echo ""
echo "# 检查内存使用"
echo "vm_stat"

echo ""
echo "⚠️  注意："
echo "- 重启进程可能会丢失未保存的工作"
echo "- 建议先保存所有工作再执行优化"
echo "- 如果问题持续，考虑重启系统"

echo ""
echo "✅ 性能优化建议完成！"
