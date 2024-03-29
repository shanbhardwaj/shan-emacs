(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
;; (when window-system
;;   ;; (menu-bar-mode -1)
;;   (tool-bar-mode -1)
;;   (scroll-bar-mode -1)
;;   (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; (add-to-list 'default-frame-alist
;;              '(fullscreen . maximized))


(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3");;; Set up package

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 0)
        ("MELPA"        . 10)))

;; (package-initialize)

(eval-when-compile
  (require 'use-package))
;; (require 'diminish)                ;; if you use :diminish
;; (require 'bind-key)                ;; if you use any :bind variant
;; (use-package general)			
(use-package diminish
  :ensure t)

;; Disable package.el in favor of straight.el
;; (setq package-enable-at-startup nil)

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;         'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (straight-use-package 'use-package)
;;(setq straight-use-package-by-default t)

;; Configure use-package to use straight.el by default
;; (use-package straight
;;   ;;:custom (straight-use-package-by-default t)
;;   )
;; (use-package general)
;; (use-package diminish)
;; (require 'general)
;; (require 'diminish)                ;; if you use :diminish
;; (require 'bind-key)

;;; Load the config
(org-babel-load-file (concat user-emacs-directory "config.org"))
(put 'erase-buffer 'disabled nil)
