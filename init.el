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
;; Moved to lisp/init-ui.el (Typography section), with the rest of the UI.

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

;; --- Linux: same modifier layout as the Mac ---------------------------------
;; The reason for this swap has changed, though the swap itself has not.
;;
;; It was written when the Mac was the KVM server and its keyboard drove this
;; machine over Deskflow.  The roles are reversed now: the 4090 has the
;; keyboard and drives the Mac over lan-mouse.  The mapping still holds
;; because the keyboard is Mac-layout -- its Command key sends LEFTMETA and
;; its Option key sends LEFTALT -- so Command must become Meta here just as it
;; is on the Mac, and Option becomes s-.
;;
;; Hyprland deliberately leaves Super alone for this: $mod is ALT there, and
;; only Command+Space and Command+Tab are taken globally, mirroring the pair
;; macOS reserves.  Without that, fourteen M- bindings were swallowed by the
;; compositor before Emacs ever saw them.
;;
;; These variables live in pgtkterm.c, so they apply to graphical frames on
;; Wayland only; TTY frames go through kkp and are untouched.  x-meta-keysym
;; is the one that fires for Alt_L on a keyboard with no dedicated Meta key,
;; which is this case; x-alt-keysym is set to match so the binding survives a
;; layout that does expose a Meta key.
(when (featurep 'pgtk)
  (setq x-super-keysym 'meta    ; Command -> M-
        x-meta-keysym  'super   ; Option  -> s-
        x-alt-keysym   'super))

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
(load "~/.emacs.d/lisp/init-speech.el")

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
