;;; init-completions.el --- Completion framework configuration -*- lexical-binding: t; -*-
(use-package consult
  :ensure t
  :bind (("M-s M-g" . consult-grep)
         ("M-s M-f" . consult-find)
         ("M-s M-o" . consult-outline)
         ("M-s M-l" . consult-line)
         ("C-x b"   . consult-buffer)
         ("C-c r"   . consult-recent-file)
         ("C-x C-r" . consult-recent-file)
         ("M-g i"   . consult-imenu)))

(use-package consult-project-extra
  :defer t
  :bind
  (("C-c p F" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(use-package consult-todo
  :demand t)

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package embark-consult
  :ensure t)

(use-package vertico
  :ensure t
  :bind (("C-x M-r" . vertico-repeat)
         :map vertico-map
         ("M-v" . vertico-multiform-vertical)
         ("M-g" . vertico-multiform-grid)
         ("M-f" . vertico-multiform-flat)
         ("M-r" . vertico-multiform-reverse)
         ("M-u" . vertico-multiform-unobtrusive))
  :init (vertico-mode 1)
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (vertico-mouse-mode 1)
  (vertico-multiform-mode 1)
  (setq vertico-count 20)
  (setq vertico-cycle t)
  (setq vertico-resize nil))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :after vertico
  :config
  (setq orderless-matching-styles '(orderless-regexp
                                    orderless-initialism
                                    orderless-prefixes)
        orderless-component-separator #'orderless-escapable-split-on-space)

  ;; Use the built-in "partial-completion" style to complete
  ;; file inputs such as "/e/ni/co.nix" into
  ;; "/etc/nixos/configuration.nix".  The "basic" style is
  ;; needed to support the hostname completion in the TRAMP
  ;; inputs such as "/sshx:HOSTNAME".
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))))
  (setq completion-styles '(orderless basic)))

(use-package cape
  :defer t
  :bind (("C-c p p" . completion-at-point)
         ("C-c p d" . cape-dabbrev)
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p r" . cape-rfc1345))
  :config
  (setq cape-dabbrev-check-other-buffers t
        cape-dabbrev-min-length 4)
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1)

  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
