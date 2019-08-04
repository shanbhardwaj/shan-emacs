(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  ;; (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(add-to-list 'default-frame-alist
             '(fullscreen . maximized))



(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3");;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib . "org"))))
(package-initialize)

;;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

;; `Paradox' is an enhanced interface for package management, which also
;; provides some helpful utility functions we're going to be using
;; extensively. Thus, the first thing we do is install it if it's not there
;; already.
(when (not (package-installed-p 'paradox))
  (package-install 'paradox))

;; First, we make sure it's installed, using a function provided by
;; Paradox, which we've just installed the hard way.
;; (paradox-require 'use-package)

;; Next, we load it so it's always available.
;; (require 'use-package)

;; From use-package README
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t)
  ;; (setq use-package-always-defer t)
  )
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)

;;; Load the config
(org-babel-load-file (concat user-emacs-directory "config.org"))
(put 'erase-buffer 'disabled nil)
