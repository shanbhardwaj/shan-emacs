;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;; --- Speed benchmarking -----------------------------------------------------
(setq init-start-time (current-time))

;; --- Identity ---------------------------------------------------------------
(setq user-full-name "Shantanu Bhardwaj"
      user-mail-address "shan@addvalsolutions.com")

;; --- Custom file ------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; --- Typography -------------------------------------------------------------
;; ;; (set-face-attribute 'default nil :height 130 :family "Monaco" :weight 'bold)
;; First family actually installed wins, so one config serves both machines:
;; SF Mono is macOS-only (Apple licence) and Caskaydia is what the Linux box
;; has.  Height travels with the family because the two displays want
;; different sizes -- hardcoding either made every `git pull' conflict.
(defvar shan/font-preferences
  '(("Liga SFMono Nerd Font"   . 140)
    ("CaskaydiaMono Nerd Font" . 120)
    ("Cascadia Code"           . 140)
    ("Menlo"                   . 140))
  "Fonts to try in order, as (FAMILY . HEIGHT).")

(defun shan/apply-font (&optional frame)
  "Set the default face to the first installed family in `shan/font-preferences'.
Runs per FRAME rather than once at startup: under the daemon, init.el is
evaluated with no frame at all, so a bare `display-graphic-p' check is nil
and the font is silently never applied."
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (let ((installed (font-family-list frame)))
        (catch 'done
          (pcase-dolist (`(,family . ,height) shan/font-preferences)
            (when (member family installed)
              (set-face-attribute 'default frame
                                  :height height :family family :weight 'regular)
              (throw 'done family))))))))

(add-hook 'after-make-frame-functions #'shan/apply-font)
(shan/apply-font)   ; non-daemon startup, where a frame already exists
;; extra space between lines, as a fraction of line height (0.2 = 20%).
;; ghostel/mu4e set this buffer-locally and those values still win.
;; (setq-default line-spacing 0.1)
;; (set-face-attribute 'bold nil :weight 'regular)
;; (set-face-attribute 'bold-italic nil :weight 'regular)

;; --- Frame / window layout --------------------------------------------------
(modify-frame-parameters nil default-frame-alist)
(setq-default pop-up-windows nil)

;; --- Modes ------------------------------------------------------------------
;; tool-bar/scroll-bar are absent from --without-x builds (the Linux box)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(blink-cursor-mode -1)
(setq recentf-max-saved-items 200)
(recentf-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(pixel-scroll-precision-mode 1)

;; --- Sane defaults ----------------------------------------------------------
(set-default-coding-systems 'utf-8)
(setq-default indent-tabs-mode nil
              ring-bell-function 'ignore
              select-enable-clipboard t)
;; --- Mode line --------------------------------------------------------------
(set-face-attribute 'mode-line nil :box nil)

;; --- macOS ------------------------------------------------------------------
(when (eq system-type 'darwin)
  ;; skip under --daemon: there is no GUI frame to focus at startup
  (when (display-graphic-p)
    (select-frame-set-input-focus (selected-frame)))
  (setq mac-option-modifier 'super
        ns-function-modifier 'super
        mac-right-command-modifier 'hyper
        mac-right-option-modifier 'alt
        mac-command-modifier 'meta)
  (setq ns-use-native-fullscreen nil)
  ;; keybindings live in init-settings.el
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash"
        ns-pop-up-frames nil))

;; --- Packages ---------------------------------------------------------------
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("elpa"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(require 'use-package)

;; keep the config dir clean: packages write state to var/ and etc/
;; (must load before anything else computes its state-file paths)
(use-package no-littering
  :ensure t
  :demand t
  :config
  ;; route backup and auto-save files into var/ instead of alongside
  ;; the files being edited
  (no-littering-theme-backups))

(use-package exec-path-from-shell
  :ensure t
  :config
  ;; login shell (-l): bash only reads .bash_profile/.bashrc for login
  ;; shells, so this is required for a correct PATH (fish was fine with
  ;; a plain non-interactive shell; bash is not)
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; --- Local packages ---------------------------------------------------------
(let* ((dir (expand-file-name (concat user-emacs-directory "local-packages")))
       (default-directory dir))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

;; --- Module loads -----------------------------------------------------------
(load "~/.emacs.d/lisp/init-builtin.el")
(load "~/.emacs.d/lisp/init-settings.el")
(load "~/.emacs.d/lisp/init-ui.el")
(load "~/.emacs.d/lisp/init-completions.el")
(load "~/.emacs.d/lisp/init-project.el")
(load "~/.emacs.d/lisp/init-packages.el")
;; mu4e lives beside the mu binary; skip where mail isn't set up (Linux box)
(when (executable-find "mu")
  (load "~/.emacs.d/lisp/init-email.el"))
(load "~/.emacs.d/lisp/llm-config.el")
(load "~/.emacs.d/lisp/term-config.el")
;; (load "~/.emacs.d/lisp/init-ruby.el")
;; (load "~/.emacs.d/lisp/init-lsp.el")
(load "~/.emacs.d/lisp/init-vc.el")
(load "~/.emacs.d/lisp/init-fold.el")
(load "~/.emacs.d/lisp/init-servers.el")
(load "~/.emacs.d/lisp/init-org.el")

;; --- Server -------------------------------------------------------------
;; normally the emacs-plus daemon serves emacsclient; if it isn't running
;; (or died), serve from this instance so Emacs Client.app and tooling
;; can always connect
(require 'server)
(unless (or (daemonp) (server-running-p))
  (server-start))

;; --- Speed benchmarking -----------------------------------------------------
(let ((init-time (float-time (time-subtract (current-time) init-start-time)))
      (total-time (string-to-number (emacs-init-time "%f"))))
  (message (concat
    (propertize "Startup time: " 'face 'bold)
    (format "%.2fs " init-time)
    (propertize (format "(+ %.2fs system time)"
                        (- total-time init-time)) 'face 'shadow))))
