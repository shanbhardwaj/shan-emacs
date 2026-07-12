;;; init-packages.el --- Package configuration -*- lexical-binding: t; -*-
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package comment-dwim-2
  :ensure t
  :bind
  ("M-;" . comment-dwim-2)
  :config
  (setopt comment-dwim-2-region-function #'comment-dwim-2-comment-or-uncomment-lines))

(use-package dabbrev
  :ensure nil
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package indent-guide
  :defer t
  :ensure t
  :hook (prog-mode . indent-guide-mode)
  :config
  (setq indent-guide-char "│"))

(use-package gcmh
  :diminish
  :hook (after-init . gcmh-mode))

(use-package diminish
  :ensure t)

(use-package neotree
  :ensure
  :defer t
  :bind ("C-<f8>" . neotree-toggle)
  :custom
  (neo-show-hidden-files t)
  (neo-theme 'nerd-icons)
  (neo-vc-integration '(face char))
  :config
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  ;; 10pt for everything in the neotree buffer, including VC-state faces
  ;; used for filenames when neo-vc-integration includes 'face
  (add-hook 'neo-after-create-hook
            (lambda (_) (face-remap-add-relative 'default :height 100))))

(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)
         ("C-c F" . helpful-function)))

(use-package hl-todo
  :ensure t
  :defer t
  :custom-face (hl-todo ((t (:box t :inherit))))
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-!"         . mc/mark-next-symbol-like-this)
         ("s-d"         . mc/mark-all-dwim)))

(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-mode)
  :config
  (setq rainbow-x-colors nil))

(use-package move-text
  :ensure t
  :bind (("M-<up>"   . move-text-up)
         ("M-<down>" . move-text-down)))

(use-package savehist
  :ensure nil
  ;; enable directly (not via after-init-hook) so an error from another
  ;; package's after-init function can't silently disable history
  :init
  ;; savehist-file location is managed by no-littering (var/savehist.el)
  ;; history-length is set in init-settings.el
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1)
  :config
  ;; cap persisted kill-ring entries so the savehist file can't balloon
  (add-to-list 'savehist-additional-variables '(kill-ring . 25)))

(use-package seethru
  :ensure t
  :commands (seethru seethru-recommended-keybinds)
  :config
  (seethru 90)
  (seethru-recommended-keybinds))

(use-package scratch
  :ensure t
  :commands scratch)

(use-package persistent-scratch
  :ensure t
  :init
  (persistent-scratch-setup-default))

(use-package smartparens
  :ensure t
  :defer t
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode))

(use-package undo-fu
  :ensure t
  :config
  (setq undo-fu-allow-undo-in-region t)
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "M-z")   'undo-fu-only-undo)
  (global-set-key (kbd "M-S-z") 'undo-fu-only-redo))

(use-package vundo
  :ensure t
  :bind ("C-M-z" . vundo))

(use-package which-key
  :defer t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

(use-package apheleia
  :ensure nil
  :defer t
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'flymake-mode)
(setq flymake-no-changes-timeout 0.5)

(use-package show-font
  :defer t)

(use-package treesit-auto
  :ensure nil
  :defer t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

(use-package restclient
  :defer t
  :ensure nil
  :commands (restclient))

(use-package yasnippet
  :ensure t
  :diminish
  :defer t
  :config
  (setq yas-snippet-dirs (concat user-emacs-directory "snippets"))
  (setq yas-indent-line 'fixed)
  (yas-global-mode)
  (global-set-key (kbd "M-/") 'company-yasnippet))
