;;; init-ruby.el --- Ruby configuration -*- lexical-binding: t; -*-
;;; Commentary:

(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Capfile\\'"
  :mode "Guardfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby"

  :init
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  (add-hook 'ruby-mode 'subword-mode)
  ;; (add-hook 'ruby-mode 'lsp-mode)

  :bind
  (([(meta down)] . ruby-forward-sexp)
   ([(meta up)]   . ruby-backward-sexp)
   ("C-c C-e"     . ruby-send-region)))  ;; Rebind since Rubocop uses C-c C-r

;; Rbenv
(use-package rbenv
  :ensure t
  :defer t
  :init (setq rbenv-show-active-ruby-in-modeline nil)
  :config (progn
            (global-rbenv-mode)
            (add-hook 'ruby-mode-hook 'rbenv-use-corresponding)))


;; Rubocop
(use-package rubocop
  :ensure t
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  :diminish cop)

;; InfRuby to change ruby
(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

;; Robe
(use-package robe
  :ensure t
  :bind ("C-M-." . robe-jump)
  :hook (ruby-mode . robe-mode)
  :config
  (defadvice inf-ruby-console-auto
      (before activate-rbenv-for-robe activate)
    (rbenv-use-corresponding))
  )

;; in buffer interpreter
(use-package seeing-is-believing
  :ensure t
  :delight
  :hook (ruby-mode . seeing-is-believing)
  :config
  (setq seeing-is-believing-max-length 150
        seeing-is-believing-max-results 10
        seeing-is-believing-timeout 10.5
        seeing-is-believing-alignment 'file))

;; Rufo Code formatter
;; (use-package rufo
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'ruby-mode-hook 'rufo-minor-mode))

;; Ruby Tools
(use-package ruby-tools
  :ensure t
  :delight
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'ruby-tools-mode))

;; RSpec
(use-package rspec-mode
  :ensure t
  :init
  (setq rspec-use-rake-when-possible nil)
  (setq rspec-spec-command "rspec")
  (setq rspec-use-spring-when-possible nil)
  (add-hook 'ruby-mode-hook 'rspec-mode)

  :config
  (defadvice rspec-compile (around rspec-compile-around)

    "Use BASH shell for running the specs because of ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))

  ;; (bind-keys :map rspec-mode-map
  ;;            ;; ("<return>" . reindent-phen-newline-and-indent)
  ;;            ("M-p ;" . rspec-poggle-spec-and-parget)
  ;;            ("M-p d" . rspec-disable-example)
  ;;            ("M-p e" . rspec-enable-example)
  ;;            ("M-p t" . rspec-verify-single)
  ;;            ("M-p l" . rspec-rerun)
  ;;            ("M-p f" . rspec-verify)
  ;;            ("M-p a" . rspec-verify-all))
  (ad-activate 'rspec-compile))

;; Ruby Refactor

(use-package ruby-refactor
  :ensure t
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

;; YAML
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))
