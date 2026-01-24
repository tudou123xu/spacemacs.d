# Security & API Key Management

为了保障您的 API Key 安全，我们采用了 Emacs 标准的 `auth-source` 机制。

## 1. 密钥存储
您的 API Key 已经存储在项目根目录下的 `.authinfo` 文件中。

**注意**：此文件包含敏感信息，**请勿**将其提交到 Git 仓库中。

文件内容格式如下：
```text
machine api.deepseek.com login deepseek password <YOUR_DEEPSEEK_KEY>
machine api.siliconflow.cn login siliconflow password <YOUR_SILICONFLOW_KEY>
```

## 2. 加密建议 (推荐)
为了更高的安全性，建议将 `.authinfo` 转换为 GPG 加密文件 `.authinfo.gpg`。
Emacs 会自动识别并解密（需要配置 GPG 环境）。

## 3. 如何使用
在 `layers/lin-ai/funcs.el` 中，我们使用 `lin-ai/get-api-key` 函数自动读取这些密钥：

```elisp
(lin-ai/get-api-key "api.deepseek.com" "deepseek")
```

如果 `.authinfo` 中找不到密钥，系统会回退尝试读取环境变量 `DEEPSEEK_API_KEY` 或 `SILICONFLOW_API_KEY`。
