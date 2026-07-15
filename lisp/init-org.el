;;; init-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;; Files live in ~/Documents/org: inbox.org is the capture target, notes.org
;; holds reference notes, and the agenda scans the whole directory.

(use-package org
  :defer t
  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o l" . org-store-link))
  :custom
  ;; Files and agenda
  (org-directory "~/Documents/org")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-agenda-files (list org-directory))
  (org-agenda-window-setup 'current-window)

  ;; Tasks
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-log-done 'time)
  (org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)

  ;; Capture
  (org-capture-templates
   '(("t" "Task" entry (file+headline "inbox.org" "Tasks")
      "* TODO %?\n%U" :empty-lines 1)
     ("n" "Note" entry (file+headline "notes.org" "Notes")
      "* %?\n%U" :empty-lines 1)))

  ;; Editing
  (org-special-ctrl-a/e t)
  (org-use-speed-commands t)
  (org-image-actual-width 550)
  (org-tags-column 45)
  (org-highlight-latex-and-related '(latex script entities))
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)

  :config
  (require 'org-tempo) ; <el TAB, <sh TAB etc. expand into src blocks
  (dolist (tpl '(("el" . "src emacs-lisp") ("rb" . "src ruby")
                 ("sh" . "src sh") ("md" . "src markdown")))
    (add-to-list 'org-structure-template-alist tpl)))

;; --- Google Calendar sync ----------------------------------------------------
;; OAuth client credentials are read from ~/.authinfo:
;;   machine org-gcal login client-id password <client-id>
;;   machine org-gcal login client-secret password <client-secret>
;; Events sync into gcal.org inside org-directory, which the agenda
;; already scans.
(use-package org-gcal
  :ensure t
  :after org
  :commands (org-gcal-sync org-gcal-fetch
             org-gcal-post-at-point org-gcal-delete-at-point)
  :bind ("C-c o g" . org-gcal-sync)
  :config
  (setq org-gcal-client-id
        (auth-source-pick-first-password :host "org-gcal" :user "client-id")
        org-gcal-client-secret
        (auth-source-pick-first-password :host "org-gcal" :user "client-secret"))
  (setq org-gcal-fetch-file-alist
        `(("shan@addvalsolutions.com"
           . ,(expand-file-name "gcal.org" org-directory))))
  (setq org-gcal-remove-api-cancelled-events t)
  ;; OAuth tokens are stored GPG-encrypted (plstore); cache the passphrase
  ;; and prompt for it in the minibuffer rather than an external pinentry
  (setq plstore-cache-passphrase-for-symmetric-encryption t)
  (setq epg-pinentry-mode 'loopback))
