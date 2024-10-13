;;; Init.el --- -*- lexical-binding: t -*-
;;; Code:

;; (eval-when-compile (defvar display-time-24hr-format t))
(eval-when-compile (defvar display-time-default-load-average nil))

;; Install and configure packages
(require 'package)

;; Add only the MELPA archive
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(require 'use-package)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Configure a package
(use-package org
  :ensure t
  :config
  (setq org-log-done 'time))

(use-package auto-package-update
  :defer t
  :custom
  (setq auto-package-update-interval 7
        auto-package-update-prompt-before-update t
        auto-package-update-hide-results nil))


(display-battery-mode t)        ;; Show battery.
(display-time-mode t)           ;; Show time.
(fset 'yes-or-no-p 'y-or-n-p)   ;; Set yes or no to y/n
(global-auto-revert-mode)       ;; Refresh a buffer if changed on disk
(global-hl-line-mode 1)         ;; Highlight current line
(blink-cursor-mode -1)          ;; Disable blingking cursor
(recentf-mode t)                ;; Turn on recent files mode

;; (pixel-scroll-precision-mode 1)
;; Enable pixel-based scrolling
;; (require 'pixel-scroll)
;; (pixel-scroll-mode 1)

;; Customize pixel-based scrolling
;; (setq pixel-scroll-precision-large-scroll-height 10)
;; (setq pixel-scroll-precision-interpolate-page t)
;; (setq pixel-scroll-precision-use-momentum t)

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

(let* ((dir (expand-file-name (concat user-emacs-directory "local-packages")))
       (default-directory dir))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

;; (add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

;; On macos use our custom settings ---------------------
(when (eq system-type 'darwin)
  ;; (set-fontset-font t nil "SF Pro Display" nil 'append)
  ;; (set-fontset-font t nil "SF Mono" nil 'append)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'super
        dired-use-ls-dired nil)
  ;; typical mac bindings
  ;; (global-set-key (kbd "M-s") 'save-buffer)
  ;; (global-set-key (kbd "M-z") 'undo)

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
  (bind-key "s-]" 'next-buffer)

  (bind-key "M-C-w" 'restart-emacs)
  )

;; Bind C-c i to open init.el
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

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

;; ========= Fonts & Themes============

(set-face-attribute 'default nil :font "Iosevka Comfy" :height 140 :weight 'regular) 
;; (set-face-attribute 'default nil :font "AnonymicePro Nerd Font" :height 160 :weight 'normal)

(defun disable-active-themes ()
  "Disables any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(bind-key "s-<f12>" 'consult-theme)
(bind-key "s-<f11>" 'disable-active-themes)

(use-package no-littering
  :ensure t
  :config
  (require 'recentf)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;; =========== Undo/Redo ============
(use-package undo-fu
  :ensure t
  :config
  (setq undo-fu-allow-undo-in-region t)
  (global-unset-key (kbd "M-z"))
  (global-set-key (kbd "M-z")   'undo-fu-only-undo)
  (global-set-key (kbd "M-S-z") 'undo-fu-only-redo))

(global-unset-key (kbd "C-z"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; rainbow-mode show hex as colors         
(use-package rainbow-mode
  :diminish
  :hook (prog-mode . rainbow-mode))

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
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)
         ("C-c F" . helpful-function)))

;; ======= Use git ========
(use-package magit
  :ensure t
  :commands (magit-status magit-ediff-show-working-tree)
  :bind ("C-c C-d" . magit-ediff-show-working-tree)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :ensure t
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 10
        magit-todos-exclude-globs '("*Pods*" ".git/" "*elpa*" "*var/lsp/*"))
  (custom-set-variables
   '(magit-todos-keywords (list "TODO" "FIXME" "HACK"))))

(use-package blamer
  :ensure t
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
  :ensure t
  :after magit
  :defer t)

(use-package orgit-forge
  :ensure t
  :after forge)

(use-package git-timemachine
  :ensure t
  :defer t
  )

(use-package git-link
  :ensure t
  :defer t)

(use-package git-gutter
  :ensure t
  :defer t
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
  :ensure t
  :defer t
  :custom-face (hl-todo ((t (:box t :inherit))))
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode))

(use-package indent-bars
  :ensure nil
  :config
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.3)
   indent-bars-pattern " . . . . ." ; play with the number of dots for your usual font size
   indent-bars-width-frac 0.25
   indent-bars-pad-frac 0.1)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
				                              list list_comprehension
				                              dictionary dictionary_comprehension
				                              parenthesized_expression subscript)))
  :hook (prog-mode . indent-bars-mode))

;; (use-package svg-tag-mode
;;   :hook (prog-mode . svg-tag-mode)
;;   :config
;;   (plist-put svg-lib-style-default :font-family "Monaco Nerd Font Mono")
;;   (plist-put svg-lib-style-default :font-size 14)
;;   ;; (require 'periphery)
;;   ;; (setq svg-tag-tags (periphery-svg-tags))
;;   )

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package toml)
(use-package gcmh
  :diminish
  :hook (after-init . gcmh-mode))
(use-package diminish
  :ensure t)

(use-package spacious-padding
  :ensure t
  :config
  ;; (setq spacious-padding-widths
  ;;       '( :internal-border-width 15
  ;;          :header-line-width 4
  ;;          :mode-line-width 6
  ;;          :tab-width 4
  ;;          :right-divider-width 30
  ;;          :scroll-bar-width 8))
  (spacious-padding-mode 1)
  )

;; Install and configure modus-themes
(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-spring t)
  )

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((ef-dream) (ef-spring)))
  (auto-dark-polling-interval-seconds 300)
  (auto-dark-allow-osascript t)
  (auto-dark-allow-powershell nil)
  ;; (auto-dark-detection-method nil) ;; dangerous to be set manually
  :hook
  (auto-dark-dark-mode
   . (lambda ()
       ;; something to execute when dark mode is detected
       ))
  (auto-dark-light-mode
   . (lambda ()
       ;; something to execute when light mode is detected
       ))
  :init (auto-dark-mode))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  ;; FIXME 2024-09-01: For some reason this stopped working because it
  ;; macroexpands to `marginalia-mode' instead of
  ;; `marginalia-mode-hook'.  What is more puzzling is that this does
  ;; not happen in the next :hook...
  ;; :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  ;; Optionally:
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          ;; ...
          (t :style "cod" :icon "code" :face font-lock-warning-face)))
  ;; Remember to add an entry for `t', the library uses that as default.
  )


(use-package yasnippet
  :ensure t
  :diminish
  :defer t
  :config
  (setq yas-snippet-dirs (concat user-emacs-directory "snippets"))
  (setq yas-indent-line 'fixed)
  (yas-global-mode)
  (global-set-key (kbd "M-/") 'company-yasnippet))

(use-package vundo
  :ensure t
  :bind ("C-M-z" . vundo))

;; (set-face-attribute 'mode-line nil
;;                     :background "LightSteelBlue1"
;;                     :foreground "black"
;;                     :box "SkyBlue2")

;; Enable display-line-numbers-mode for prog-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enable flymake-mode for prog-mode
(add-hook 'prog-mode-hook 'flymake-mode)

;; Customize line number display
(setq display-line-numbers-type 'relative) ; Use relative line numbers

;; Customize flymake-mode
(setq flymake-no-changes-timeout 0.5) ; Set the timeout for flymake to 0.5 seconds


;;========== Completions ==========
;; The `vertico' package applies a vertical layout to the minibuffer.
;; It also pops up the minibuffer eagerly so we can see the available
;; options without further interactions.  This package is very fast
;; and "just works", though it also is highly customisable in case we
;; need to modify its behaviour.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:cff33514-d3ac-4c16-a889-ea39d7346dc5
(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; The `marginalia' package provides helpful annotations next to
;; completion candidates in the minibuffer.  The information on
;; display depends on the type of content.  If it is about files, it
;; shows file permissions and the last modified date.  If it is a
;; buffer, it shows the buffer's size, major mode, and the like.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:bd3f7a1d-a53d-4d3e-860e-25c5b35d8e7e
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;; The `orderless' package lets the minibuffer use an out-of-order
;; pattern matching algorithm.  It matches space-separated words or
;; regular expressions in any order.  In its simplest form, something
;; like "ins pac" matches `package-menu-mark-install' as well as
;; `package-install'.  This is a powerful tool because we no longer
;; need to remember exactly how something is named.
;;
;; Note that Emacs has lots of "completion styles" (pattern matching
;; algorithms), but let us keep things simple.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:7cc77fd0-8f98-4fc0-80be-48a758fcb6e2
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

;; The `consult' package provides lots of commands that are enhanced
;; variants of basic, built-in functionality.  One of the headline
;; features of `consult' is its preview facility, where it shows in
;; another Emacs window the context of what is currently matched in
;; the minibuffer.  Here I define key bindings for some commands you
;; may find useful.  The mnemonic for their prefix is "alternative
;; search" (as opposed to the basic C-s or C-r keys).
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:22e97b4c-d88d-4deb-9ab3-f80631f9ff1d
(use-package consult
  :ensure t
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))

(use-package consult-project-extra
  :defer t
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(use-package consult-todo
  :demand t)

;; The `embark' package lets you target the thing or context at point
;; and select an action to perform on it.  Use the `embark-act'
;; command while over something to find relevant commands.
;;
;; When inside the minibuffer, `embark' can collect/export the
;; contents to a fully fledged Emacs buffer.  The `embark-collect'
;; command retains the original behaviour of the minibuffer, meaning
;; that if you navigate over the candidate at hit RET, it will do what
;; the minibuffer would have done.  In contrast, the `embark-export'
;; command reads the metadata to figure out what category this is and
;; places them in a buffer whose major mode is specialised for that
;; type of content.  For example, when we are completing against
;; files, the export will take us to a `dired-mode' buffer; when we
;; preview the results of a grep, the export will put us in a
;; `grep-mode' buffer.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:61863da4-8739-42ae-a30f-6e9d686e1995
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

;; The `embark-consult' package is glue code to tie together `embark'
;; and `consult'.
(use-package embark-consult
  :ensure t)

;; The `wgrep' packages lets us edit the results of a grep search
;; while inside a `grep-mode' buffer.  All we need is to toggle the
;; editable mode, make the changes, and then type C-c C-c to confirm
;; or C-c C-k to abort.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852
(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

;; The built-in `savehist-mode' saves minibuffer histories.  Vertico
;; can then use that information to put recently selected options at
;; the top.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:25765797-27a5-431e-8aa4-cc890a6a913a
(savehist-mode 1)

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
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;;; Corfu (in-buffer completion popup)
(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  ;; I also have (setq tab-always-indent 'complete) for TAB to complete
  ;; when it does not need to perform an indentation change.
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package treemacs
  :ensure t
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

(use-package project-treemacs
  :demand t
  :after treemacs
  )

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

(use-package mise
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-mise-mode)) 

;; ========== js settings ========

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (ruby-mode . ruby-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))


;; ============= LSP ==============

(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode
           js-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flymake)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

  :init
  (setq lsp-use-plists t)

  :config
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection '("typescript-language-server" "--stdio"))
  ;;                   :major-modes '(typescript-ts-mode)
  ;;                   :server-id 'ts-ls))
  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)

       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (setq lsp-use-plists t)
  ;; Initiate https://github.com/blahgeek/emacs-lsp-booster for performance
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;; ====== end lsp-mode 

;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook ((typescript-mode . lsp-deferred)
;;          (typescript-ts-mode . lsp-deferred)
;;          (js-mode . lsp-deferred)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :config
;;   (setq lsp-typescript-server 'typescript-javascript-server)
;;   ;; Optional: Specify the path to the TypeScript language server
;;   (setq lsp-typescript-server-path "/opt/homebrew/bin/typescript-language-server")
;;   ;; Optional: Enable logging for troubleshooting
;;   (setq lsp-log-level 'debug))


(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode evil)
  :config (setq lsp-ui-doc-enable t
                evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))

(use-package lsp-eslint
  :demand t
  :after lsp-mode
  :config
  (setq lsp-eslint-server-command '("eslint-lsp" "--stdio")))

(use-package lsp-tailwindcss
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

(use-package apheleia
  :ensure t
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

;; ==== Mu4e =====
(use-package mu4e
  :load-path  "/opt/homebrew/Cellar/mu/1.12.6/share/emacs/site-lisp/mu/mu4e/"
  :config
  (setq mu4e-maildir "~/Mail")
  
  (setq mu4e-mu-binary (executable-find "mu"))
  ;; this command is called to sync imap servers:
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  ;; how often to call it in seconds:
  (setq mu4e-update-interval 300)

  ;; save attachment to desktop by default
  (setq mu4e-attachment-dir "~/Downloads")

  ;; rename files when moving - needed for mbsync:
  (setq mu4e-change-filenames-when-moving t)

  ;; list of your email adresses:
  (setq mu4e-user-mail-address-list '("bhardwaj.10@gmail.com"
                                      "shan@addvalsolutions.com"
                                      "shantanu@kulcare.com")))

(require 'mu4e)

(setq mu4e-maildir-shortcuts
      '(("/Kulcare/Inbox"             . ?k)
        ("/Addval/Inbox"              . ?a)
        ("/Gmail/Inbox"               . ?g)
        ("/Kulcare/[Gmail]/Sent Mail" . ?s)
        ("/Kulcare/[Gmail]/Trash"     . ?t)
        ("/Kulcare/[Gmail]/Drafts"    . ?d)
        ("/Kulcare/[Gmail]/All Mail"  . ?l)))

;; the following is to show shortcuts in the main view.
(add-to-list 'mu4e-bookmarks
             '(:name "Inbox - Gmail"
                     :query "maildir:/gmail/INBOX"
                     :key ?g))
(add-to-list 'mu4e-bookmarks
             '(:name "Inbox - Addval"
                     :query "maildir:/addval/INBOX"
                     :key ?a))
(add-to-list 'mu4e-bookmarks
             '(:name "Inbox - Kulcare"
                     :query "maildir:/kulcare/INBOX"
                     :key ?k))

(setq mu4e-contexts
      '(,
        ;; Addval account
        (make-mu4e-context
         :name "Addval"
         :match-func
         (lambda (msg)
           (when msg
             (string-prefix-p "/Addval" (mu4e-message-field msg :maildir))))
         :vars '((user-mail-address . "shan@addvalsolutions.com")
                 (user-full-name    . "Shantanu Bhardwaj")
                 (smtpmail-smtp-server  . "smtp.gmail.com")
                 (smtpmail-smtp-service . 465)
                 (smtpmail-stream-type  . ssl)
                 (mu4e-drafts-folder  . "/Addval/[Gmail]/Drafts")
                 (mu4e-sent-folder  . "/Addval/[Gmail]/Sent Mail")
                 (mu4e-refile-folder  . "/Addval/[Gmail]/All Mail")
                 (mu4e-trash-folder  . "/Addval/[Gmail]/Trash")))

        ;; Kulcare account
        ,(make-mu4e-context
          :name "Kulcare"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Kulcare" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "shantanu@kulcare.com")
                  (user-full-name    . "Shantanu Bhardwaj")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/Kulcare/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/Kulcare/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/Kulcare/[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/Kulcare/[Gmail]/Trash")))

        ;; ;; Codetiger account
        ;; ,(make-mu4e-context
        ;;  :name "Codetiger"
        ;;  :match-func
        ;;  (lambda (msg)
        ;;    (when msg
        ;;      (string-prefix-p "/Codetiger" (mu4e-message-field msg :maildir))))
        ;;  :vars '((user-mail-address . "shan@codetiger.com")
        ;;          (user-full-name    . "Shan Bhardwaj")
        ;;          (smtpmail-smtp-server  . "smtp.gmail.com")
        ;;          (smtpmail-smtp-service . 465)
        ;;          (smtpmail-stream-type  . ssl)
        ;;          (mu4e-drafts-folder  . "/Codetiger/[Gmail]/Drafts")
        ;;          (mu4e-sent-folder  . "/Codetiger/[Gmail]/Sent Mail")
        ;;          (mu4e-refile-folder  . "/Codetiger/[Gmail]/All Mail")
        ;;          (mu4e-trash-folder  . "/Codetiger/[Gmail]/Trash")))

        ;; Gmail account
        ,(make-mu4e-context
          :name "Gmail"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "bhardwaj.10@gmail.com")
                  (user-full-name    . "Shantanu Bhardwaj")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/Gmail/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/Gmail/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/Gmail/[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash")))
        ))


;; sending email
;; --------------

;; Make sure plain text mails flow correctly for recipients
(setq mu4e-compose-format-flowed t)

;; don't keep message compose buffers around after sending:
(setq message-kill-buffer-on-exit t)

;; Only ask if a context hasn't been previously picked
(setq mu4e-compose-context-policy 'ask-if-none)

;; send function:
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

;; send program:
;; this is exeranal. remember we installed it before.
(setq sendmail-program (executable-find "msmtp"))

;; select the right sender email from the context.
(setq message-sendmail-envelope-from 'header)


;;==== end of init ======
