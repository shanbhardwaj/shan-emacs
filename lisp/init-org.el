;;; init-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;; Files live in the Orgenda iOS app's iCloud folder, so the phone and Emacs
;; edit the same set: inbox.org is the capture target, notes.org holds
;; reference notes, and the agenda scans the whole directory.
;; global-auto-revert-mode (init.el) picks up edits made on the phone.
;; Caveat: editing the same file in both places at once makes iCloud write
;; a conflict copy -- finish on one side before the other.

(use-package org
  :defer t
  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o n" . shan/org-open-notes)
         ("C-c o i" . shan/org-open-inbox)
         ("C-c o f" . shan/org-open-file))
  :custom
  ;; Files and agenda
  (org-directory "~/Library/Mobile Documents/iCloud~com~yakshaven~orgenda/Documents")
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
  ;; flat top-level entries, matching how Orgenda writes its files
  (org-capture-templates
   '(("t" "Task" entry (file "inbox.org")
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

;; --- Quick access to org files -----------------------------------------------
(defun shan/org-open-notes ()
  "Open notes.org in `org-directory'."
  (interactive)
  (find-file (expand-file-name "notes.org" org-directory)))

(defun shan/org-open-inbox ()
  "Open inbox.org in `org-directory'."
  (interactive)
  (find-file (expand-file-name "inbox.org" org-directory)))

(defun shan/org-open-file ()
  "Pick any file in `org-directory' with completion."
  (interactive)
  (let ((default-directory (file-name-as-directory org-directory)))
    (call-interactively #'find-file)))

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
