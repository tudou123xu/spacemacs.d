#!/bin/bash
# 安全的 Emacs 启动脚本 - 完全禁用原生编译

# 设置环境变量
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export EMACS_NO_NATIVE_COMP=1

# 启动 Emacs 并强制禁用原生编译
exec emacs \
    --eval "(setq native-comp-deferred-compilation nil)" \
    --eval "(setq native-comp-jit-compilation nil)" \
    --eval "(setq native-comp-eln-load-path nil)" \
    --eval "(setq native-comp-enable-subr-trampolines nil)" \
    --eval "(setq native-comp-speed 0)" \
    --eval "(setq comp-async-report-warnings-errors nil)" \
    --eval "(setq comp-deferred-compilation nil)" \
    --eval "(setq comp-deferred-compilation-black-list '(\".*\"))" \
    --eval "(setq comp-deferred-compilation-deny-list '(\".*\"))" \
    --load "$HOME/.spacemacs.d/early-init.el" \
    "$@"
