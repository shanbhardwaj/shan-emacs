;;; Init.el --- -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile (defvar display-time-24hr-format t))
(eval-when-compile (defvar display-time-default-load-average nil))

;; (set-face-attribute 'default nil :font "Iosevka Comfy" :height 160 :weight 'thin) 
;; (set-face-attribute 'fixed-pitch nil :font "Iosevka Comfy" :height 140 :weight 'thin)
;; (set-face-attribute 'variable-pitch nil :font "Iosevka Comfy" :height 140 :weight 'light)

;; (custom-set-faces
;;  '(font-lock-comment-face ((t (:font "Iosevka Comfy" :italic t :height 1.0)))))
(set-face-attribute 'default nil :font "AnonymicePro Nerd Font" :height 160 :weight 'normal)
;; (set-face-attribute 'fixed-pitch nil :font "Monaco" :height 140 :weight 'thin)
;; (set-face-attribute 'variable-pitch nil :font "Monaco" :height 140 :weight 'light)

;; (custom-set-faces
;;  '(font-lock-comment-face ((t (:font "Monaco" :italic t :height 1.0)))))

(display-battery-mode t)        ;; Show battery.
(display-time-mode t)           ;; Show time.
(fset 'yes-or-no-p 'y-or-n-p)   ;; Set yes or no to y/n
(global-auto-revert-mode)       ;; refresh a bufFer if changed on disk
(global-hl-line-mode 1)         ;; Highlight current line
(blink-cursor-mode -1)          ;; disable blingking cursor

;; (pixel-scroll-precision-mode 1)

(setq ad-redefinition-action            'accept
      global-auto-revert-non-file-buffers t
      auto-revert-check-vc-info         t
      backup-by-copying                 t
      backup-directory-alist            '(("." . "~/.emacs.d/backups"))
      cursor-in-non-selected-windows    nil
      byte-compile-warnings             '(ck-functions)
      confirm-kill-processes            nil
      create-lockfiles                  nil
      echo-keystrokes                   0.2
      confirm-kill-emacs                'y-or-n-p
      column-number-mode                t
      find-file-visit-truename          t
      font-lock-maximum-decoration      t
      highlight-nonselected-windows     t
      fast-but-imprecise-scrolling      t
      jit-lock-defer-time               nil
      kill-buffer-query-functions       nil    ;; Dont ask for closing spawned processes
      scroll-margin                     0   ;; scroll N to screen edge
      load-prefer-newer                 t
      use-dialog-box                    nil
      visible-bell                      nil
      word-wrap                         nil
      auto-mode-case-fold               nil
      truncate-lines                    t
      truncate-string-ellipsis          ".."
      undo-limit                        6710886400 ;; 64mb
      undo-strong-limit                 100663296 ;; x 1.5 (96mb)
      undo-outer-limit                  1006632960) ;; x 10 (960mb), (Emacs uses x100), but this seems too high.

(setq-default display-line-numbers-width    3       ;; Set so we can display thousands of lines
              c-basic-offset                2            ;; Set tab indent for c/c++ to 4 tabs
              ediff-forward-word-function   'forward-char
              ediff-split-window-function   'split-window-horizontally
              tab-width                     2            ;: Use four tabs
              indent-tabs-mode              nil			 ;; Never use tabs. Use spaces instead
              truncate-lines                t
              indent-line-function          'insert-tab  ;; Use function to insert tabs
              history-length                100
              uniquify-buffer-name-style    'forward)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(let* ((dir (expand-file-name (concat user-emacs-directory "local-packages")))
       (default-directory dir))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

                                        ; On macos use our custom settings ---------------------
(when (eq system-type 'darwin)
  ;; (set-fontset-font t nil "SF Pro Display" nil 'append)
  ;; (set-fontset-font t nil "SF Mono" nil 'append)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'super
        dired-use-ls-dired nil)
  ;; typical mac bindings
  (global-set-key (kbd "M-s") 'save-buffer)
  (global-set-key (kbd "M-z") 'undo)

  ;; Use Command-` to switch between Emacs windows (not frames)
  (bind-key "A-`" 'other-window)

  ;; Use Command-Shift-` to switch Emacs frames in reverse
  (bind-key "s-~" (lambda() () (interactive) (other-window -1)))

  ;; Because of the keybindings above, set one for `other-frame'
  (bind-key "S-1" 'other-frame)

  ;; Fullscreen!
  (setq ns-use-native-fullscreen nil) ; Not Lion style
  (bind-key "<s-return>" 'toggle-frame-fullscreen)

  ;; buffer switching
  (bind-key "s-[" 'previous-buffer)
  (bind-key "s-]" 'next-buffer))

(defun vsplit-other-window ()
  "Splits the window vertically and switches to that window."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))
(defun hsplit-other-window ()
  "Splits the window horizontally and switches to that window."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))

(bind-key "C-x 2" 'vsplit-other-window)
(bind-key "C-x 3" 'hsplit-other-window)

(put 'narrow-to-page 'disabled nil)

;; Dont leave #file autosaves everywhere I go
(defvar my-auto-save-folder (concat user-emacs-directory "var/auto-save/"))
(setq auto-save-list-file-prefix (concat my-auto-save-folder ".saves-")
      auto-save-file-name-transforms `((".*", my-auto-save-folder t))
      custom-file (concat user-emacs-directory "var/custom.el"))

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)

(use-package use-package
  :ensure nil
  :config
  (setq use-package-verbose t
        use-package-expand-minimally t
        use-package-always-ensure t
        use-package-compute-statistics t
        use-package-minimum-reported-time 0.1
        debug-on-error nil))

(use-package gcmh
  :diminish
  :hook (after-init . gcmh-mode))

(use-package autothemer
  :config
  (load-theme 'moe-dark t)
  ;; (load-theme 'catppuccin-latte t)
  ;; (load-theme 'gruvbox-dark-hard t)
  ;; (load-theme 'wombat t)
  ;; (load-theme 'catppuccin-frappe t)
  ;; (load-theme 'catppuccin-macchiato t)
  ;; (load-theme 'catppuccin-mocha t)
  ;; (load-theme 'rose-pine t)
  ;; (load-theme 'oxocarbon t)
  ;; (load-theme 'oxographite t)
  ;; (load-theme 'kman t)
  ;; (load-theme 'kanagawa t)
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-all)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-checker-simple-format nil)
  ;; (set-face-attribute 'mode-line nil :height 0.9)
  ;; (set-face-attribute 'mode-line-inactive nil :height 0.9)
  ;; (setq doom-modeline-height 12)
  )

(defun switch-theme (theme)
  "Disables any currently active themes and loads THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (reset-modeline)
    ))

(defun disable-active-themes ()
  "Disables any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(bind-key "s-<f12>" 'consult-theme)
(bind-key "s-<f11>" 'disable-active-themes)

(use-package ace-jump-mode
  :ensure t
  :diminish ace-jump-mode
  :commands ace-jump-mode
  :bind ("C-S-s" . ace-jump-mode))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package auto-package-update
  :custom
  (setq auto-package-update-interval 7
        auto-package-update-prompt-before-update t
        auto-package-update-hide-results nil))

(use-package undo-fu
  :config
  (setq undo-fu-allow-undo-in-region t)
  (global-unset-key (kbd "M-z"))
  (global-set-key (kbd "M-z")   'undo-fu-only-undo)
  (global-set-key (kbd "M-S-z") 'undo-fu-only-redo))

(use-package svg-tag-mode
  :hook (prog-mode . svg-tag-mode)
  :config
  (plist-put svg-lib-style-default :font-family "Monaco Nerd Font Mono")
  (plist-put svg-lib-style-default :font-size 14)
  ;; (require 'periphery)
  ;; (setq svg-tag-tags (periphery-svg-tags))
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; rainbow-mode show hex as colors         
(use-package rainbow-mode
  :diminish
  :hook (prog-mode . rainbow-mode))

;; (use-package tree-sitter
;;   :hook (swift-mode . tree-sitter-mode)
;;   :config
;;   (setq tsc-dyn-get-from nil)
;;   (setq tree-sitter-hl-use-font-lock-keywords t
;;         tree-sitter-hl-enable-query-region-extension nil)
;;   :config
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package smooth-scrolling
  :ensure t)

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

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ;; ("C-x C-c" . describe-char)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))


(use-package vertico
  :ensure t
  :demand
  :config
  (setq vertico-cycle t)
  ;; currently requires melpa version of vertico
  (setq vertico-preselect 'directory)
  :init
  (vertico-mode)
  (defun my/vertico-insert ()
    (interactive)
    (let* ((mb (minibuffer-contents-no-properties))
           (lc (if (string= mb "") mb (substring mb -1))))
      (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
            ((file-directory-p (vertico--candidate)) (vertico-insert))
            (t (self-insert-command 1 ?/)))))
  :bind (:map vertico-map
              ("/" . #'my/vertico-insert)))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; :demand
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; (use-package vertico
;;   :hook (after-init . vertico-mode)
;;   :bind
;;   (:map vertico-map
;;         ("C-j" . vertico-next)
;;         ("C-k" . vertico-previous)
;;         ("C-d" . vertico-scroll-down)
;;         ("C-u" . vertico-scroll-up))
;;   :custom
;;   (vertico-buffer-display-action '(display-buffer-reuse-window))
;;   :config
;;   (vertico-multiform-mode)
;;   (setq vertico-resize t
;;         vertico-count 8
;;         vertico-multiline nil
;;         vertico-scroll-margin 4
;;         vertico-cycle t
;;         read-file-name-completion-ignore-case t
;;         read-buffer-completion-ignore-case t
;;         completion-ignore-case t))

;; ;; Configure directory extension.
;; (use-package vertico-directory
;;   :commands (find-file)
;;   :ensure nil
;;   :bind (:map vertico-map
;;               ("<tab>" . vertico-directory-enter)
;;               ("DEL" . vertico-directory-delete-char)
;;               ("M-DEL" . vertico-directory-delete-word))
;;   ;; Tidy shadowed file names
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-posframe
  :after vertico
  :init
  (vertico-posframe-mode 1)
  (vertico-posframe-cleanup)
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 0)
          (right-fringe . 0)))
  (setq ;; vertico-posframe-poshandler #'posframe-poshandler-frame-top-left-corner
   ;; vertico-posframe-poshandler #'posframe-poshandler-frame-top-center
   ;; vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center
   vertico-posframe-poshandler #'posframe-poshandler-frame-center ;
   vertico-posframe-truncate-lines t
   vertico-posframe-min-width 120
   vertico-posframe-width 155
   vertico-posframe-min-height 2
   vertico-posframe-border-width 2))


(use-package nerd-icons
  :defer t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package orderless
  :after vertico
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles flex))
                                        (eglot (styles . (orderless flex))))))

(use-package marginalia
  :after (vertico)
  :config (marginalia-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ;; ("C-s" . (lambda () (interactive) (consult-line (thing-at-point 'symbol))))
  ("M-l" . consult-goto-line)
  ("<backtab>" . consult-buffer)
  ("C-c C-a" . consult-apropos)
  ("C-c m m" . consult-imenu-multi)
  ("M-O" . consult-ls-git)
  ("M-g" . consult-line))

(use-package embark-consult
  :after (embark consult))
``
(use-package consult-projctile
  :after projectile)

(use-package recentf
  :hook (after-init . recentf-mode))

;; ------------------ SEARCHING -------------------
(use-package rg
  :defer t)

;; ------------------ EDITING -------------------
;; (use-package consult-project-extra
;;   :after consult
;;   :bind
;;   ("C-<tab>" . #'consult-projectile-switch-to-buffer))

;; (use-package consult-ls-git
;;   :after consult)

(use-package google-this
  :commands (google-this)
  :bind ("C-x C-g" . google-this))

(use-package corfu
  :hook ((prog-mode . corfu-mode)
         (localizeable-mode . corfu-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("<escape>" . corfu-quit)
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous))
  :custom
  (corfu-auto t)
  (completion-styles '(flex orderless))
  :init
  (setq corfu-bar-width 2
        corfu-scroll-margin 2
        corfu-auto-prefix 1
        corfu-min-width 40
        corfu-max-width 130
        corfu-left-margin-width 0.9
        corfu-right-margin-width 0.9
        corfu-bar-width 0.2
        corfu-count 14
        corfu-auto-delay 0.5
        corfu-quit-no-match 'separator
        corfu-popupinfo-delay 0.5
        corfu-popupinfo-resize t
        corfu-popupinfo-hide nil
        corfu-popupinfo-direction '(force-horizontal)
        corfu-popupinfo-resize t
        corfu-popupinfo-min-width corfu-min-width
        corfu-popupinfo-max-width corfu-max-width
        tab-always-indent 'complete))

;; (use-package corfu-history
;;   :ensure nil
;;   :after (corfu savehist)
;;   :config
;;   (corfu-history-mode 1)
;;   (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode t))

(use-package dabbrev
  :ensure nil
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Add extensions
(use-package cape
  :defer t
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p r" . cape-rfc1345))
  :custom
  (setq cape-dabbrev-check-other-buffers t
        cape-dabbrev-min-length 4)
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package projectile
  :diminish
  :hook (prog-mode . projectile-mode)
  :bind
  ("C-M-r" . projectile-replace)
  :custom
  (setq projectile-completion-system 'auto
        projectile-enable-caching t
        projectile-sort-order 'default
        projectile-indexing-method 'hybrid
        projectile-verbose nil
        projectile-switch-project-action #'projectile-commander
        projectile-ignored-files '(".orig$" ".yml$"))
  :config
  (add-to-list 'projectile-globally-ignored-directories "build")
  (setq projectile-globally-ignored-directories
        '(".git"
          "swiftpm"
          "pods"
          "xcodeproj"
          ".build")))


;; Use git
(use-package magit
  :commands (magit-status magit-ediff-show-working-tree)
  :bind ("C-c C-d" . magit-ediff-show-working-tree)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 10
        magit-todos-exclude-globs '("*Pods*" ".git/" "*elpa*" "*var/lsp/*"))
  (custom-set-variables
   '(magit-todos-keywords (list "TODO" "FIXME" "HACK"))))

(use-package blamer
  :commands (blamer-mode)
  :config
  (setq blamer-view 'overlay-right
        blamer-type 'visual
        blamer-max-commit-message-length 70
        blamer-force-truncate-long-line nil
        blamer-author-formatter " ✎ %s "
        blamer-commit-formatter "● \'%s\' ● ")
  :custom
  (blamer-idle-time 1.0)
  :custom-face
  (blamer-face ((t :foreground "#E46876"
                   :height 130
                   :bold t
                   :italic t))))

(use-package forge
  :after magit
  :defer t)

(use-package orgit-forge
  :after forge)

(use-package git-timemachine)

(use-package git-link
  :defer t)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :diminish git-gutter-mode
  :config
  (setq git-gutter:update-interval 1))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (setq git-gutter-fr:side 'left-fringe)
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated)))

(use-package hl-todo
  :defer t
  :custom-face (hl-todo ((t (:box t :inherit))))
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode))


(use-package crux
  :ensure t
  :bind (("C-c o o" . crux-open-with)
         ("C-c o u" . crux-view-url)
         ("C-a" . crux-move-beginning-of-line)
         ("C-c f" . crux-recentf-find-file)))

(use-package treemacs
  :commands (treemacs treemacs-select-window)
  :hook (treemacs-mode . treemacs-project-follow-mode)
  :bind ("M-J" . treemacs-find-file)
  :custom-face
  (doom-themes-treemacs-file-face ((t (:weight semi-bold))))
  (treemacs-file-face ((t (:family "Monaco Nerd Font Mono"))))
  (treemacs-root-face ((t (:family "Monaco Nerd Font Mono" :height 0.9))))
  (treemacs-directory-face ((t (:family "Monaco Nerd Font Mono" :height 0.9))))
  (treemacs-directory-collapsed-face ((t (:family "Monaco Nerd Font Mono" :height 0.9))))
  (treemacs-git-ignored-face ((t (:family "Monaco Nerd Font Mono" :height 0.9))))
  (treemacs-git-unmodified-face ((t (:family "Monaco Nerd Font Mono" :height 0.9))))
  (treemacs-git-untracked-face ((t (:family "Monaco Nerd Font Mono" :height 0.9))))
  (treemacs-git-added-face ((t (:family "Monaco Nerd Font Mono" :height 0.9))))
  (treemacs-git-renamed-face ((t (:family "Monaco Nerd Font Mono" :height 0.9))))
  (treemacs-git-modified-face ((t (:family "Monaco Nerd Font Mono" :height 0.9))))
  (treemacs-tags-face ((t (:family "Monaco Nerd Font Mono" :height 0.9))))
  :config
  (setq doom-themes-treemacs-theme nil
        treemacs-window-background-color '("#221F22" . "#423f42"))
  (setq treemacs-follow-after-init t
        treemacs-collapse-dirs 1
        treemacs-directory-name-transformer #'identity
        treemacs-file-name-transformer #'identity
        treemacs-show-cursor nil
        treemacs-display-current-project-exclusively t
        treemacs-filewatch-mode t
        treemacs-follow-mode nil
        treemacs-hide-dot-git-directory t
        treemacs-git-integration t
        treemacs-space-between-root-nodes t
        treemacs-hide-gitignored-files-mode t
        treemacs-git-mode 'extended
        treemacs-indentation 1
        treemacs-is-never-other-window t
        treemacs-silent-refresh	t
        treemacs-sorting 'treemacs--sort-alphabetic-case-insensitive-asc
        treemacs-width 30))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package restclient
  :commands (restclient))

(use-package flyspell
  :ensure nil
  :config
  (setq flyspell-issue-message-flag nil
        ispell-local-dictionary "sv-SE"
        ispell-program-name "aspell"))
(provide 'init)

(use-package indent-bars
  :ensure nil
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
				                              list list_comprehension
				                              dictionary dictionary_comprehension
				                              parenthesized_expression subscript)))
  :hook (prog-mode . indent-bars-mode))

(use-package vterm
  :commands vterm
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (setq vterm-timer-delay nil))

(use-package web-mode
  :ensure t
  :mode (("\\.ts\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.mjs\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :hook (web-mode . eglot-ensure)
  :config
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'"))))

;; --- Debugging
(use-package dape
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; :preface
  ;; (setq dape-key-prefix "\C-x\C-a")
  ;;
  ;; May also need to set/change gud (gdb-mi) key prefix
  ;; (setq gud-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
   (after-init . dape-breakpoint-load))

  :init
  ;; To use window configuration like gud (gdb-mi)
  (setq dape-buffer-window-arrangement 'gud)

  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root)
  )

;; --- JS dev

(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)

;; --- Ruby dev 

;; lsp 
(with-eval-after-load 'eglot
 (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))


;;; init.el ends here
;; (put 'narrow-to-region 'disabled nil)
