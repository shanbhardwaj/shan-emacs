(setq user-full-name "Shantanu Bhardwaj"
      user-mail-address "shan@addvalsolutions.com")

(setq max-lisp-eval-depth 10000)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(menu-bar-mode -1)

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

;;  (load-theme 'gruvbox-dark-hard t)
  
  (defun reset-modeline()
    "Reset the modeline to 12pt font"
    (interactive)
    (set-face-attribute 'mode-line nil :height 100)
    (set-face-attribute 'mode-line-inactive nil :height 100)
    (setq doom-modeline-height 12)
    (set-face-attribute 'line-number nil :font "Monaco-10")
    (set-face-attribute 'line-number-current-line nil :font "Monaco-10")
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

  (bind-key "s-<f12>" 'switch-theme)
  (bind-key "s-<f11>" 'disable-active-themes)

(add-to-list 'default-frame-alist
             ;; '(font . "-*-Operator Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
             '(font . "Monaco Nerd Font Mono-16")
             ;; '(font . "Cascadia Mono-14")
             )

(with-current-buffer " *Echo Area 0*" (face-remap-add-relative 'default '(:family "Monaco" :height 110)))

;; Use monospaced font faces in current buffer
(defun my-term-mode-face ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Monaco" :height 100))
  (buffer-face-mode))

;; These functions are useful. Activate them.
 (put 'downcase-region 'disabled nil)
 (put 'upcase-region 'disabled nil)
 (put 'narrow-to-region 'disabled nil)
 (put 'dired-find-alternate-file 'disabled nil)

 ;; Answering just 'y' or 'n' will do
 (defalias 'yes-or-no-p 'y-or-n-p)

 ;; Keep all backup and auto-save files in one directory
 ;; (setq backup-directory-alist '(("*" . "~/.emacs.d/backups")))
 ;; (setq auto-save-file-name-transforms '(("." "~/.emacs.d/auto-save-list/" t)))
 (setq make-backup-files nil)

 ;; UTF-8 please
 (setq locale-coding-system 'utf-8) ; pretty
 (set-terminal-coding-system 'utf-8) ; pretty
 (set-keyboard-coding-system 'utf-8) ; pretty
 (set-selection-coding-system 'utf-8) ; please
 (prefer-coding-system 'utf-8) ; with sugar on top
 (setq-default indent-tabs-mode nil)

 ;; Turn off the blinking cursor
 ;; (blink-cursor-mode -1)

 (setq-default indent-tabs-mode nil)
 (setq-default indicate-empty-lines t)

 ;; Don't count two spaces after a period as the end of a sentence.
 ;; Just one space is needed.
 (setq sentence-end-double-space nil)

 ;; delete the region when typing, just like as we expect nowadays.
 (delete-selection-mode t)

 (show-paren-mode t)

 (column-number-mode t)

 ;; (global-visual-line-mode -1)
 (remove-hook 'text-mode-hook #'turn-on-auto-fill)
 (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
 (diminish 'visual-line-mode)

 (setq uniquify-buffer-name-style 'forward)

 ;; -i gets alias definitions from .bash_profile
 (setq shell-command-switch "-ic")

 ;; Don't beep at me
 (setq visible-bell nil)

 ;; highlight current line everywhere
 (global-hl-line-mode 1)

 (global-unset-key (kbd "M-m"))
 (global-set-key (kbd "C-+") 'text-scale-increase)
 (global-set-key (kbd "C--") 'text-scale-decrease)
                   ;; (global-set-key (kbd "C-+") 'text-scale-increase)
                   ;; (global-set-key (kbd "C--") 'text-scale-decrease)

 (add-hook 'prog-mode-hook #'display-line-numbers-mode)
 (windmove-default-keybindings 'super)
(setq native-comp-async-report-warnings-errors 'silent)

(when (string-equal system-type "darwin")

  (add-to-list 'default-frame-alist
               '(ns-transparent-titlebar . t))
  ;; set the window frame to dark theme
  (add-to-list 'default-frame-alist
               '(ns-appearance . dark))

  ;; delete files by moving them to the trash
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")
  ;; Don't make new frames when opening a new file with Emacs
  (setq ns-pop-up-frames nil)

  ;; set the Fn key as the hyper key
  ;; (setq ns-function-modifier 'hyper)
  ;; (setq ns-option-modifier 'super)
  ;; (setq ns-command-modifier 'meta)

  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)

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
  (bind-key "M-[" 'previous-buffer)
  (bind-key "M-]" 'next-buffer)

  ;; Compiling
  (bind-key "H-c" 'compile)
  (bind-key "H-r" 'recompile)
  (bind-key "H-s" (defun save-and-recompile () (interactive) (save-buffer) (recompile)))

  ;; disable the key that minimizes emacs to the dock because I don't
  ;; minimize my windows
  ;; (global-unset-key (kbd "C-z"))

  ;; Not going to use these commands
  (put 'ns-print-buffer 'disabled t)
  (put 'suspend-frame 'disabled t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-!"         . mc/mark-next-symbol-like-this)
         ("s-d"         . mc/mark-all-dwim)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-mode projectile-switch-project)
  :init
  ;; (setq projectile-completion-system 'vertico)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (global-set-key (kbd "C-c p p") 'projectile-switch-project)
  (setq projectile-switch-project-action #'projectile-find-dir)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

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

(use-package undo-tree
  :ensure t
  :defer t
  :diminish
  :init
  (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package savehist
  :ensure t
  :init
  (setq history-length 25)
  (savehist-mode))

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
            (setq vertico-count 20) ;; number of candidates and also size of minibuffer
            (setq vertico-multiform-categories '((consult-grep buffer))
                  vertico-multiform-commands '((tmm-menubar flat)
                                               (tmm-shortcut flat)))))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ;; left/right arrows for changing directory:
              ("<right>"   . vertico-directory-enter)
              ("<left>"    . vertico-directory-delete-word)
              ("M-<left>"  . vertico-directory-delete-char))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))



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


;; https://github.com/minad/marginalia
(use-package marginalia
  :ensure t
  :after vertico
  ;; :demand t
  :config (marginalia-mode 1))
