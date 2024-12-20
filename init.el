(setq user-full-name "Shantanu Bhardwaj"
      user-mail-address "shan@addvalsolutions.com")

;; Add only the MELPA archive
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(require 'use-package)
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package auto-package-update
  :defer t
  :custom
  (setq auto-package-update-interval 7
        auto-package-update-prompt-before-update t
        auto-package-update-hide-results nil))

(let* ((dir (expand-file-name (concat user-emacs-directory "local-packages")))
       (default-directory dir))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

(use-package emacs
  :ensure nil 
  :custom                                         ;; Set custom variables to configure Emacs behavior.
  (column-number-mode t)                          ;; Display the column number in the mode line.
  (auto-save-default nil)                         ;; Disable automatic saving of buffers.
  (create-lockfiles nil)                          ;; Prevent the creation of lock files when editing.
  (delete-by-moving-to-trash t)                   ;; Move deleted files to the trash instead of permanently deleting them.
  (delete-selection-mode 1)                       ;; Enable replacing selected text with typed text.
  (display-line-numbers-type 'relative)           ;; Use relative line numbering in programming modes.
  (global-auto-revert-non-file-buffers t)         ;; Automatically refresh non-file buffers.
  (history-length 25)                             ;; Set the length of the command history.
  (ispell-dictionary "en_US")                     ;; Set the default dictionary for spell checking.
  (make-backup-files nil)                         ;; Disable creation of backup files.
  (pixel-scroll-precision-mode t)                 ;; Enable precise pixel scrolling.
  (pixel-scroll-precision-use-momentum nil)       ;; Disable momentum scrolling for pixel precision.
  (ring-bell-function 'ignore)                    ;; Disable the audible bell.
  (split-width-threshold 300)                     ;; Prevent automatic window splitting if the window width exceeds 300 pixels.
  (switch-to-buffer-obey-display-actions t)       ;; Make buffer switching respect display actions.
  (tab-always-indent 'complete)                   ;; Make the TAB key complete text instead of just indenting.
  (tab-width 2)                                   ;; Set the tab width to 4 spaces.
  (treesit-font-lock-level 4)                     ;; Use advanced font locking for Treesit mode.
  (truncate-lines t)                              ;; Enable line truncation to avoid wrapping long lines.
  (use-dialog-box nil)                            ;; Disable dialog boxes in favor of minibuffer prompts.
  (use-short-answers t)                           ;; Use short answers in prompts for quicker responses (y instead of yes)
  (warning-minimum-level :emergency)              ;; Set the minimum level of warnings to display.
  (uniquify-buffer-name-style    'forward)

  :hook                                           ;; Add hooks to enable specific features in certain modes.
  (prog-mode . display-line-numbers-mode)         ;; Enable line numbers in programming modes.

  :config
  ;; By default emacs gives you access to a lot of *special* buffers, while navigating with [b and ]b,
  ;; This settings make sure ]b and [b will always load a file buffer. 
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  ;; Save manual customizations to a separate file instead of cluttering `init.el'.
  (setq custom-file (locate-user-emacs-file "custom-vars.el")) ;; Specify the custom file path.
  (load custom-file 'noerror 'nomessage)                       ;; Load the custom file quietly, ignoring errors.

  ;; Makes Emacs vertical divisor the symbol │ instead of |.
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  :init                        ;; Initialization settings that apply before the package is loaded.
  (tool-bar-mode -1)           ;; Disable the tool bar for a cleaner interface.
  (menu-bar-mode -1)           ;; Disable the menu bar for a more streamlined look.

  (when scroll-bar-mode
    (scroll-bar-mode -1))      ;; Disable the scroll bar if it is active.

  (global-hl-line-mode 1)      ;; Enable highlight of the current line 
  (global-auto-revert-mode 1)  ;; Enable global auto-revert mode to keep buffers up to date with their corresponding files.
  (indent-tabs-mode -1)        ;; Disable the use of tabs for indentation (use spaces instead).
  (recentf-mode 1)             ;; Enable tracking of recently opened files.
  (savehist-mode 1)            ;; Enable saving of command history.
  (save-place-mode 1)          ;; Enable saving the place in files for easier return.
  (winner-mode 1)              ;; Enable winner mode to easily undo window configuration changes.
  (xterm-mouse-mode 1)         ;; Enable mouse support in terminal mode.
  (file-name-shadow-mode 1)    ;; Enable shadowing of filenames for clarity.

  ;; Set the default coding system for files to UTF-8.
  (modify-coding-system-alist 'file "" 'utf-8)

  ;; Add a hook to run code after Emacs has fully initialized.
  (add-hook 'after-init-hook
  					(lambda ()
  						(message "Emacs has fully loaded. This code runs after startup.")

  						;; Insert a welcome message in the *scratch* buffer displaying loading time and activated packages.
  						(with-current-buffer (get-buffer-create "*scratch*")
  							(insert (format
  											 ";;    Welcome to Emacs!
  ;;
  ;;    Loading time : %s
  ;;    Packages     : %s
  "
  											 (emacs-init-time)
  											 (number-to-string (length package-activated-list))))))))

(use-package window
  :ensure nil       ;; This is built-in, no need to fetch it.
  :custom
  (display-buffer-alist
   '(
     ;; ("\\*.*e?shell\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . -1))
     
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Example configuration for the LSP help buffer,
     ;; keeps it always on bottom using 25% of the available space:
     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
     
     ;; Configuration for displaying various diagnostic buffers on
     ;; bottom 25%:
     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     )))

(use-package dired
  :ensure nil                                                ;; This is built-in, no need to fetch it.
  :custom
  (dired-listing-switches "-lah --group-directories-first")  ;; Display files in a human-readable format and group directories first.
  (dired-dwim-target t)                                      ;; Enable "do what I mean" for target directories.
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open") ;; Open image files with `feh' or the default viewer.
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open") ;; Open audio and video files with `mpv'.
     (".*" "open" "xdg-open")))                              ;; Default opening command for other files.
  (dired-kill-when-opening-new-dired-buffer t)               ;; Close the previous buffer when opening a new `dired' instance.
  :config
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))                     ;; Use GNU ls on macOS if available.
      (when gls
        (setq insert-directory-program gls)))))

;; (use-package dired-hacks
;;   :after dired
;;   :ensure nil)

(use-package erc
  :defer t ;; Load ERC when needed rather than at startup. (Load it with `M-x erc RET')
  :custom
  (erc-join-buffer 'window)                                        ;; Open a new window for joining channels.
  (erc-hide-list '("JOIN" "PART" "QUIT"))                          ;; Hide messages for joins, parts, and quits to reduce clutter.
  (erc-timestamp-format "[%H:%M]")                                 ;; Format for timestamps in messages.
  (erc-autojoin-channels-alist '((".*\\.libera\\.chat" "#emacs"))));; Automatically join the #emacs channel on Libera.Chat.

(use-package isearch
  :ensure nil                                  ;; This is built-in, no need to fetch it.
  :config
  (setq isearch-lazy-count t)                  ;; Enable lazy counting to show current match information.
  (setq lazy-count-prefix-format "(%s/%s) ")   ;; Format for displaying current match count.
  (setq lazy-count-suffix-format nil)          ;; Disable suffix formatting for match count.
  (setq search-whitespace-regexp ".*?")        ;; Allow searching across whitespace.
  :bind (("C-s" . isearch-forward)             ;; Bind C-s to forward isearch.
         ("C-r" . isearch-backward)))          ;; Bind C-r to backward isearch.

(use-package vc
  :ensure nil                        ;; This is built-in, no need to fetch it.
  :defer t
  :bind
  (("C-x v d" . vc-dir)              ;; Open VC directory for version control status.
   ("C-x v =" . vc-diff)             ;; Show differences for the current file.
   ("C-x v D" . vc-root-diff)        ;; Show differences for the entire repository.
   ("C-x v v" . vc-next-action))     ;; Perform the next version control action.
  :config
  ;; Better colors for <leader> g b  (blame file) 
  (setq vc-annotate-color-map
        '((20 . "#f5e0dc")
          (40 . "#f2cdcd")
          (60 . "#f5c2e7")
          (80 . "#cba6f7")
          (100 . "#f38ba8")
          (120 . "#eba0ac")
          (140 . "#fab387")
          (160 . "#f9e2af")
          (180 . "#a6e3a1")
          (200 . "#94e2d5")
          (220 . "#89dceb")
          (240 . "#74c7ec")
          (260 . "#89b4fa")
          (280 . "#b4befe"))))

(use-package smerge-mode
  :ensure nil                                  ;; This is built-in, no need to fetch it.
  :defer t
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)  ;; Keep the changes from the upper version.
              ("C-c ^ l" . smerge-keep-lower)  ;; Keep the changes from the lower version.
              ("C-c ^ n" . smerge-next)        ;; Move to the next conflict.
              ("C-c ^ p" . smerge-previous)))  ;; Move to the previous conflict.

(use-package eldoc
  :ensure nil          ;; This is built-in, no need to fetch it.
  :init
  (global-eldoc-mode))

;; TODO: add eldoc-box

(use-package flymake
  :ensure nil          ;; This is built-in, no need to fetch it.
  :defer t
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error) (warning "»" compilation-warning)
     (note "»" compilation-info))))

(use-package which-key
  :ensure nil     ;; This is built-in, no need to fetch it.
  :defer t        ;; Defer loading Which-Key until after init.
  :hook
  (after-init . which-key-mode)) ;; Enable which-key mode after initialization.

(defun align-values (start end)
  "Vertically aligns region based on lengths of the first value of each line.
    Example output:

            foo        bar
            foofoo     bar
            foofoofoo  bar"
  (interactive "r")
  (align-regexp start end
                "\\([a-z_]+: \\)"
                -1 1 nil))

;; (defun fontify-frame (frame)
;;   (interactive)
;;   (if sys/macp
;;       (progn
;;         (if (> (x-display-pixel-width) 4000)
;;             (set-frame-parameter frame 'font "Monaco 14") ;; 5k Display
;;           (set-frame-parameter frame 'font "Monaco 12")))))

;; ;; Fontify current frame
;; (fontify-frame nil)

;; ;; Fontify any future frames
;; (push 'fontify-frame after-make-frame-functions)

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

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
  			doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Install and configure modus-themes
(use-package ef-themes
  :ensure nil)

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((doom-one) (doom-one-light)))
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


;; (defun reset-modeline()
;;   "Reset the modeline to 12pt font"
;;   (interactive)
;;   (set-face-attribute 'mode-line nil :height 100)
;;   (set-face-attribute 'mode-line-inactive nil :height 100)
;;   (setq doom-modeline-height 12)
;;   (set-face-attribute 'line-number nil :font "Monaco-10")
;;   (set-face-attribute 'line-number-current-line nil :font "Monaco-10")
;;   )

;; (defun switch-theme (theme)
;;   "Disables any currently active themes and loads THEME."
;;   ;; This interactive call is taken from `load-theme'
;;   (interactive
;;    (list
;;     (intern (completing-read "Load custom theme: "
;;                              (mapc 'symbol-name
;;                                    (custom-available-themes))))))
;;   (let ((enabled-themes custom-enabled-themes))
;;     (mapc #'disable-theme custom-enabled-themes)
;;     (load-theme theme t)
;;     (reset-modeline)
;;     ))

(defun disable-active-themes ()
  "Disables any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(bind-key "s-<f12>" 'consult-theme)
(bind-key "s-<f11>" 'disable-active-themes)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom    
  (doom-modeline-height 20)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-time-analogue-clock nil)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (setq doom-modeline-github t)
  (setq doom-modeline-battery-icon nil)
  (setq doom-modeline-mu4e t)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil))

(set-face-attribute 'default nil :font "Iosevka Comfy" :height 140 :weight 'regular) 
;; (set-face-attribute 'default nil :font "CaskaydiaMono Nerd Font" :height 140 :weight 'regular)

(with-current-buffer " *Echo Area 0*" (face-remap-add-relative 'default '(:family "Monaco" :height 110)))

;; Use monospaced font faces in current buffer
(defun my-term-mode-face ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Monaco" :height 100))
  (buffer-face-mode))

;; Don't count two spaces after a period as the end of a sentence.
;; Just one space is needed.
(setq sentence-end-double-space nil)

;; -i gets alias definitions from .bash_profile
(setq shell-command-switch "-ic")

(global-unset-key (kbd "M-m"))
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; Bind C-c i to open init.el
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.org")))

(windmove-default-keybindings 'super)

(when (string-equal system-type "darwin")

  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper
        dired-use-ls-dired nil)

  ;; Fullscreen!
  (setq ns-use-native-fullscreen nil) ; Not Lion style
  (bind-key "<s-return>" 'toggle-frame-fullscreen)

  ;; buffer switching
  (bind-key "s-[" 'previous-buffer)
  (bind-key "s-]" 'next-buffer)

  (bind-key "M-C-w" 'restart-emacs)


  ;; delete files by moving them to the trash
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")
  (setq ns-pop-up-frames nil)

  ;; Compiling
  (bind-key "H-c" 'compile)
  (bind-key "H-r" 'recompile)
  (bind-key "H-s" (defun save-and-recompile () (interactive) (save-buffer) (recompile))))

(use-package dabbrev
  :ensure nil
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package indent-guide
  :defer t
  :ensure t
  :hook
  (prog-mode . indent-guide-mode)  ;; Activate indent-guide in programming modes.
  :config
  (setq indent-guide-char "│"))    ;; Set the character used for the indent guide.

;; (use-package svg-tag-mode
;;   :hook (prog-mode . svg-tag-mode)
;;   :config
;;   (plist-put svg-lib-style-default :font-family "Monaco Nerd Font Mono")
;;   (plist-put svg-lib-style-default :font-size 14)
;;   ;; (require 'periphery)
;;   ;; (setq svg-tag-tags (periphery-svg-tags))
;;   )

(use-package toml)
(use-package gcmh
  :diminish
  :hook (after-init . gcmh-mode))
(use-package diminish
  :ensure t)

(use-package neotree
  :ensure t
  :custom
  (neo-show-hidden-files t)                ;; By default shows hidden files (toggle with H)
  (neo-theme 'icons)                        ;; Set the default theme for Neotree to 'nerd' for a visually appealing look.
  (neo-vc-integration '(face char))        ;; Enable VC integration to display file states with faces (color coding) and characters (icons).
  :defer t)                                 ;; Load the package only when needed to improve startup time.

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

(use-package no-littering
  :ensure t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (require 'recentf)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  )

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

;; Further reading: https://protesilaos.com/emacs/dotemacs#h:25765797-27a5-431e-8aa4-cc890a6a913a
  ;;;; `savehist' (minibuffer and related histories)
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

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

(use-package super-save
  :ensure t
  :defer t
  :config
  ;; (super-save-mode +1)
  )

(use-package undo-fu
  :ensure t
  :config
  (setq undo-fu-allow-undo-in-region t)
  (global-unset-key (kbd "M-z"))
  (global-set-key (kbd "M-z")   'undo-fu-only-undo)
  (global-set-key (kbd "M-S-z") 'undo-fu-only-redo))

(global-unset-key (kbd "C-z"))

(use-package vundo
  :ensure t
  :bind ("C-M-z" . vundo))

;; (set-face-attribute 'mode-line nil
;;                     :background "LightSteelBlue1"
;;                     :foreground "black"
;;                     :box "SkyBlue2")

;; (use-package undo-tree
;;   :ensure t
;;   :defer t
;;   :diminish
;;   :init
;;   (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

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

(use-package org
  :config
  (require 'org-tempo)
  )

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-babel-safelist '(
                                         "~/.emacs.d/init.org"
                                         )))

(bind-key "C-c l" 'org-store-link)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c a" 'org-agenda)

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/Documents/Agenda"))))

(bind-key "C-c c" 'org-capture)
(setq org-default-notes-file "~/Documents/Notes/notes.org")

(setq org-use-speed-commands t)
(setq org-image-actual-width 550)
(setq org-highlight-latex-and-related '(latex script entities))

(setq org-startup-indented 'f)
(setq org-directory "~/Documents/Apps/Org")
(setq org-special-ctrl-a/e 't)
(setq org-default-notes-file (concat org-directory "/Notes.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-mobile-directory "~/Documents/Apps/MobileOrg")
(setq org-src-fontify-natively 't)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)

(setq org-agenda-files (quote ("~/Documents/Apps/Org/Inbox.org"
                               ;;                                "~/Documents/Apps/Org/Addval.org"
                               ;;                                "~/Documents/Apps/Org/Brandbin.org"
                               ;;                                "~/Documents/Apps/Org/Kulcare.org"
                               )))

(setq org-tags-column 45)

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp" ))
(add-to-list 'org-structure-template-alist '("rb" . "src ruby" ))
(add-to-list 'org-structure-template-alist '("sh" . "src sh" ))
(add-to-list 'org-structure-template-alist '("md" . "src markdown"))

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

(use-package vertico
  :ensure t
  :bind (("C-x M-r" . vertico-repeat)
         :map vertico-map
         ;; M- keys for changing view:
         ("M-v"     . vertico-multiform-vertical)
         ("M-g"     . vertico-multiform-grid)
         ("M-f"     . vertico-multiform-flat)
         ("M-r"     . vertico-multiform-reverse)
         ("M-u"     . vertico-multiform-unobtrusive))
  :init (vertico-mode 1)
  :config (progn
            (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
            (vertico-mouse-mode 1)
            (vertico-multiform-mode 1)
            (setq vertico-count 20)
            (setq vertico-cycle t)
            (setq vertico-resize nil)))

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

;; https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :after vertico
  :config (progn
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
            (setq completion-styles '(orderless basic))))

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

;; (use-package company
;;   :defer t 
;;   :ensure t
;;   :custom
;;   (company-tooltip-align-annotations t)      ;; Align annotations with completions.
;;   (company-minimum-prefix-length 1)          ;; Trigger completion after typing 1 character
;;   (company-idle-delay 0.2)                   ;; Delay before showing completion (adjust as needed)
;;   (company-tooltip-maximum-width 50) 
;;   :config

;;   ;; While using C-p C-n to select a completion candidate
;;   ;; C-y quickly shows help docs for the current candidate
;;   (define-key company-active-map (kbd "C-y")
;; 			  (lambda ()
;; 				(interactive)
;; 				(company-show-doc-buffer)))
;;   (define-key company-active-map [tab] 'company-complete-selection)
;;   (define-key company-active-map (kbd "TAB") 'company-complete-selection)
;;   (define-key company-active-map [ret] 'company-complete-selection)
;;   (define-key company-active-map (kbd "RET") 'company-complete-selection)
;;   :hook
;;   (after-init . global-company-mode)) ;; Enable Company Mode globally after initialization.

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

;; Enable display-line-numbers-mode for prog-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enable flymake-mode for prog-mode
(add-hook 'prog-mode-hook 'flymake-mode)

;; Customize line number display
(setq display-line-numbers-type 'relative) ; Use relative line numbers

;; Customize flymake-mode
(setq flymake-no-changes-timeout 0.5) ; Set the timeout for flymake to 0.5 seconds

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

(use-package mise
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-mise-mode))

(use-package treesit-auto
  :ensure t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           js-ts-mode
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
  ;;                   :major-modes '(js-ts-mode)
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

(use-package lsp-eslint
  :demand t
  :after lsp-mode
  :config
  (setq lsp-eslint-server-command '("eslint-lsp" "--stdio")))

(use-package lsp-tailwindcss
  :ensure t
  :defer t
  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.erb$" . "html")) ;; Associate ERB files with HTML.
  :init
  (setq lsp-tailwindcss-add-on-mode t))



(use-package restclient
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

(use-package mu4e
  :load-path  "/opt/homebrew/Cellar/mu/1.12.6/share/emacs/site-lisp/mu/mu4e/"
  :custom
  (mu4e-use-fancy-chars nil)
  (mu4e-mu-binary (executable-find "mu"))
  (mu4e-get-command (concat (executable-find "mbsync") " -a"))
  (mu4e-update-interval 300)
  (mu4e-maildir "~/Mail")
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-change-filenames-when-moving t)
  ;; === send email ===
  (mu4e-compose-format-flowed t)
  (message-kill-buffer-on-exit t)
  (mu4e-compose-context-policy 'ask-if-none)
  (send-mail-function 'sendmail-send-it
                      message-send-mail-function 'sendmail-send-it)
  (sendmail-program (executable-find "msmtp"))
  (message-sendmail-envelope-from 'header)
  :hook
  ((mu4e-view-mode . visual-line-mode)
   (mu4e-compose-mode . (lambda ()
                          (use-hard-newlines -1)
                          (flyspell-mode)))
   (mu4e-headers-mode . (lambda ()
                          (interactive)
                          (setq mu4e-headers-fields
                                `((:human-date . 25) ;; alternatively, use :date
                                  (:flags . 6)
                                  (:from . 25)
                                  (:thread-subject . ,(- (window-body-width) 75)) ;; alternatively, use :subject
                                  (:size . 7))))))
  :init
  (use-package mu4e-thread-folding
    :load-path (lambda () (expand-file-name "site-elisp/mu4e-thread-folding/" user-emacs-directory))
    :after mu4e
    :bind
    ((:map mu4e-headers-mode-map
           ("TAB" . mu4e-headers-toggle-at-point)
           ("C-<tab>" . mu4e-headers-toggle-fold-all))
     (:map mu4e-search-minor-mode-map
           ("S" . mu4e-kill-update-mail)))
    :custom
    (mu4e-thread-folding-default-view `folded)
    (mu4e-headers-fields '((:empty         .    2)
                           (:human-date    .   12)
                           (:flags         .    6)
                           (:mailing-list  .   10)
                           (:from          .   25)
                           (:subject       .   nil))))
  :config  
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
      `(,
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

        ;; Codetiger account
        ,(make-mu4e-context
          :name "Codetiger"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Codetiger" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "shan@codetiger.com")
                  (user-full-name    . "Shan Bhardwaj")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/Codetiger/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/Codetiger/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/Codetiger/[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/Codetiger/[Gmail]/Trash")))

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



(use-package mu4e-alert
  :hook (after-init-hook . mu4e-alert-enable-mode-line-display)
  :config
  (setq mu4e-alert-email-notification-types '(count)))

(use-package mu4e-marker-icons)
(use-package mu4e-column-faces
  :after mu4e
  :config (mu4e-column-faces-mode))

;; (use-package mu4e-dashboard
;;    )

(use-package mu4e-views
  :after mu4e)
