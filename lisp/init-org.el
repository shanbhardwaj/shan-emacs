;;; init-org.el --- Org mode configuration -*- lexical-binding: t; -*-
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
