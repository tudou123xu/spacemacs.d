(defun ellama/init-ellama ()
  (use-package ellama
    :init
    (setq ellama-provider 'ollama    ;; 使用本地 Ollama 服务
          ellama-model "llama3:8b"   ;; 默认模型
          ellama-api-base "http://localhost:11434")
    :config
    (spacemacs/set-leader-keys
      "oel" #'ellama-chat-new
      "oer" #'ellama-region-rewrite)))
