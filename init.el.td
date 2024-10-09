;;; Init.el --- -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile (defvar display-time-24hr-format t))
(eval-when-compile (defvar display-time-default-load-average nil))

;; Install and configure packages
(require 'package)

;; Add only the MELPA archive
(setq package-archives
      (list (cons "melpa" "https://melpa.org/packages/")))

(package-initialize)

(require 'use-package)

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
(global-auto-revert-mode)       ;; refresh a bufFer if changed on disk
(global-hl-line-mode 1)         ;; Highlight current line
(blink-cursor-mode -1)          ;; disable blingking cursor

;; (pixel-scroll-precision-mode 1)
;; Enable pixel-based scrolling
(require 'pixel-scroll)
(pixel-scroll-mode 1)

;; Customize pixel-based scrolling
(setq pixel-scroll-precision-large-scroll-height 10)
(setq pixel-scroll-precision-interpolate-page t)
(setq pixel-scroll-precision-use-momentum t)

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

;; ========= Fonts & Themes============

(set-face-attribute 'default nil :font "Iosevka Comfy" :height 140 :weight 'regular) 
;; (set-face-attribute 'default nil :font "AnonymicePro Nerd Font" :height 160 :weight 'normal)

(defun disable-active-themes ()
  "Disables any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;; (bind-key "s-<f12>" 'consult-theme)
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

(use-package svg-tag-mode
  :hook (prog-mode . svg-tag-mode)
  :config
  (plist-put svg-lib-style-default :font-family "Monaco Nerd Font Mono")
  (plist-put svg-lib-style-default :font-size 14)
  ;; (require 'periphery)
  ;; (setq svg-tag-tags (periphery-svg-tags))
  )

;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; rainbow-mode show hex as colors         
;; (use-package rainbow-mode
;;   :diminish
;;   :hook (prog-mode . rainbow-mode))

;; (use-package smooth-scrolling
;;   :ensure t)

(use-package scratch
  :ensure t
  :commands scratch)

;; (use-package persistent-scratch
;;   :ensure t
;;   :init
;;   (persistent-scratch-setup-default))

;; (use-package smartparens
;;   :ensure t
;;   :defer t
;;   :diminish smartparens-mode
;;   :hook (prog-mode . smartparens-mode))

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

;; (use-package git-gutter
;;   :ensure t
;;   :defer t
;;   :hook (prog-mode . git-gutter-mode)
;;   :diminish git-gutter-mode
;;   :config
;;   (setq git-gutter:update-interval 1))

;; (use-package git-gutter-fringe
;;   :after git-gutter
;;   :config
;;   (setq git-gutter-fr:side 'left-fringe)
;;   (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated)))

;; (use-package hl-todo
;;   :defer t
;;   :custom-face (hl-todo ((t (:box t :inherit))))
;;   :bind (:map hl-todo-mode-map
;;               ([C-f3] . hl-todo-occur)
;;               ("C-c t p" . hl-todo-previous)
;;               ("C-c t n" . hl-todo-next)
;;               ("C-c t o" . hl-todo-occur))
;;   :hook (after-init . global-hl-todo-mode))

;; (use-package indent-bars
;;   :ensure nil
;;   :custom
;;   (indent-bars-treesit-support t)
;;   (indent-bars-no-descend-string t)
;;   (indent-bars-treesit-ignore-blank-lines-types '("module"))
;;   (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
;; 				                              list list_comprehension
;; 				                              dictionary dictionary_comprehension
;; 				                              parenthesized_expression subscript)))
;;   :hook (prog-mode . indent-bars-mode))


;;==== end of init ======
