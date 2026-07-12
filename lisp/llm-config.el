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
  :commands (agent-shell-anthropic-start-claude-code
             agent-shell-openai-start-codex)
  :bind (("C-c a" . agent-shell-anthropic-start-claude-code)
         ("C-c A" . agent-shell-openai-start-codex))
  :config
  ;; both agents use subscription logins, not API keys:
  ;; claude auth for Claude Code, ChatGPT login (codex login) for Codex
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t)))

(use-package agent-shell-dashboard
  ;; not on MELPA; linked from the official agent-shell README
  :vc (:url "https://github.com/wandersoncferreira/agent-shell-dashboard" :rev :newest)
  :after agent-shell
  :commands (agent-shell-dashboard)
  :bind ("C-c d" . agent-shell-dashboard)
  :config
  (setq agent-shell-dashboard-summary-command
        '("claude" "-p" "--allowed-tools" "" "--model" "haiku"))
  (setq agent-shell-dashboard-recent-sessions-count 8)

  (defun my/claude-cli-session-cwd (file)
    "Return the cwd recorded in Claude Code transcript FILE, or nil."
    (with-temp-buffer
      (insert-file-contents file nil 0 65536)
      (goto-char (point-min))
      (when (re-search-forward "\"cwd\":\"\\([^\"]+\\)\"" nil t)
        (match-string 1))))

  (defun my/agent-shell-dashboard-recent-sessions ()
    "Recent sessions from agent-shell transcripts plus the Claude CLI store.
Merges the dashboard's default source with sessions found under
~/.claude/projects/ so sessions started in other terminals show up too."
    (let* ((defaults (agent-shell-dashboard--recent-sessions-default))
           (live (agent-shell-dashboard--live-session-ids))
           (seen (make-hash-table :test 'equal))
           (root (expand-file-name "~/.claude/projects/"))
           ;; UUID-named files only; skips agent-*.jsonl subagent transcripts.
           (files (and (file-directory-p root)
                       (directory-files-recursively
                        root
                        "\\`[0-9a-f]\\{8\\}\\(?:-[0-9a-f]\\{4\\}\\)\\{3\\}-[0-9a-f]\\{12\\}\\.jsonl\\'")))
           (out (copy-sequence defaults)))
      (dolist (s defaults)
        (puthash (plist-get s :id) t seen))
      ;; Newest files first; only parse a handful to keep refresh cheap.
      (setq files (seq-take
                   (seq-sort-by
                    (lambda (f)
                      (float-time (file-attribute-modification-time
                                   (file-attributes f))))
                    #'> files)
                   25))
      (dolist (f files)
        (let ((id (file-name-base f))
              (mtime (float-time (file-attribute-modification-time
                                  (file-attributes f)))))
          (unless (or (gethash id seen) (gethash id live))
            (when-let* ((cwd (my/claude-cli-session-cwd f)))
              (puthash id t seen)
              (push (list :id id :cwd cwd :agent "Claude Code"
                          :opened mtime :time mtime)
                    out)))))
      (seq-take (seq-sort-by (lambda (s) (plist-get s :time)) #'> out)
                agent-shell-dashboard-recent-sessions-count)))

  (setq agent-shell-dashboard-recent-sessions-function
        #'my/agent-shell-dashboard-recent-sessions))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'eat)) ; Optionally enable Emacs MCP tools

(provide 'llm-config)
