#!/bin/bash
###
 # @Author: xuzhifeng xuzhifeng@hellogroup.com
 # @Date: 2025-09-22 18:46:19
 # @LastEditors: xuzhifeng xuzhifeng@hellogroup.com
 # @LastEditTime: 2025-09-23 10:46:08
 # @FilePath: /.spacemacs.d/scripts/start-emacs-safe.sh
 # @Description: 这是默认设置,请设置`customMade`, 打开koroFileHeader查看配置 进行设置: https://github.com/OBKoro1/koro1FileHeader/wiki/%E9%85%8D%E7%BD%AE
### 
# 安全的 Emacs 启动脚本 - 修复原生编译问题

# 使用完全禁用原生编译的启动器
exec "$HOME/.spacemacs.d/scripts/emacs-no-native" "$@"