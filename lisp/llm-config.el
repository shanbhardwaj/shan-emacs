;;; llm-config.el --- LLM configuration -*- lexical-binding: t; -*-
(require 'use-package) ;; Ensure `use-package` is loaded

(use-package gptel
  :ensure t
  :custom
  ;; key is read from ~/.authinfo:
  ;;   machine api.openai.com login apikey password <your-key>
  (gptel-api-key #'gptel-api-key-from-auth-source)
  (gptel-model "gpt-4o")
  :bind
  (("C-c g q" . gptel-query)
   ("C-c g r" . gptel-response))
  :config
  (setq gptel-max-tokens 2048))

(use-package agent-shell
  :ensure t
  :commands (agent-shell-anthropic-start-claude-code)
  :bind ("C-c a" . agent-shell-anthropic-start-claude-code)
  :config
  ;; use the Claude subscription login (claude auth), not an API key
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'eat)) ; Optionally enable Emacs MCP tools

(provide 'llm-config)
