;;; init-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;; Files live in ~/org on every machine: inbox.org is the capture target,
;; notes.org holds reference notes, and the agenda scans the whole directory.
;; Syncthing replicates the folder between the Mac and the 4090, and
;; global-auto-revert-mode (init.el) picks up what it writes.
;; Caveat: editing the same file on both machines at once leaves a
;; .sync-conflict-* file beside it -- finish on one side before the other.

(use-package org
  :defer t
  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o n" . shan/org-open-notes)
         ("C-c o i" . shan/org-open-inbox)
         ("C-c o f" . shan/org-open-file)
         ("C-c o G" . shan/org-refresh-github-issues))
  :custom
  ;; Files and agenda
  ;; Plain ~/org, not an app's iCloud container: that path does not exist on
  ;; Linux, so agenda and capture were dead on the 4090.  Syncthing keeps the
  ;; two machines in step; a cloud client can watch the same folder later for
  ;; phone access, without tying the files to one app again.
  (org-directory "~/org")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-agenda-files (list org-directory))
  (org-agenda-window-setup 'current-window)

  ;; One view that hides nothing: C-c o a d.
  ;; The plain agenda (a) only lists items with a date, so undated captures
  ;; from inbox.org are invisible there -- which is most of what the Leader
  ;; Key capture produces.  This stacks today's dated items on top of the
  ;; undated backlog, newest captures first.
  (org-agenda-custom-commands
   '(("d" "Dashboard: today + undated inbox"
      ((agenda "" ((org-agenda-span 'day)
                   (org-deadline-warning-days 7)))
       (todo "NEXT"
             ((org-agenda-overriding-header "In progress")))
       ;; -github keeps the 50-odd issues in github.org out of here; they
       ;; get their own section below.  Without it they drown the inbox.
       (tags-todo "-SCHEDULED={.}-DEADLINE={.}-github/!TODO"
                  ((org-agenda-overriding-header "Inbox (no date)")
                   (org-agenda-sorting-strategy '(timestamp-down))))
       (tags-todo "+github/!TODO"
                  ((org-agenda-overriding-header "GitHub (assigned to me)")
                   (org-agenda-max-entries 12)))
       (todo "WAITING"
             ((org-agenda-overriding-header "Waiting on someone")))))))

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
      "* %?\n%U" :empty-lines 1)
     ;; Filled by org-protocol (see org-capture-safari / bookmarklet):
     ;; %:description = page title, %:link = URL, %i = selected text.
     ;; Point lands at %? so you type WHY you saved it -- that one line is
     ;; the difference between this and a bookmark graveyard.
     ("w" "Web link" entry (file "inbox.org")
      "* TODO %?\n%U\n[[%:link][%:description]]\n\n%i" :empty-lines 1)))

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
    (add-to-list 'org-structure-template-alist tpl))

  ;; org-protocol: lets anything outside Emacs hand a URL/title/selection
  ;; to org-capture by running
  ;;   emacsclient "org-protocol://capture?template=w&url=...&title=..."
  ;; No macOS URL-scheme registration needed for the ~/.local/bin/org-capture-safari
  ;; script (it calls emacsclient directly); registration is only required if
  ;; a browser bookmarklet should launch it.
  (require 'org-protocol)
  ;; capture frames are small and disposable: give them their own frame and
  ;; delete it on finish/abort, so capturing never disturbs a window layout
  (defun shan/org-capture-frame-p ()
    (equal (frame-parameter nil 'name) "org-capture"))
  (defun shan/org-capture-cleanup-frame ()
    (when (shan/org-capture-frame-p) (delete-frame)))
  (add-hook 'org-capture-after-finalize-hook #'shan/org-capture-cleanup-frame)
  ;; in the dedicated capture frame, show ONLY the capture buffer -- the new
  ;; frame otherwise inherits whatever buffer the daemon had current
  (defun shan/org-capture-only-window ()
    (when (shan/org-capture-frame-p) (delete-other-windows)))
  (add-hook 'org-capture-mode-hook #'shan/org-capture-only-window))

;; --- Entry counts on agenda section headers ----------------------------------
;; The block headers are static strings, and a block cannot know its own
;; result count while it is being built -- so count afterwards, once the
;; buffer is rendered, and append [n] to each header.  Sections are marked
;; with the `org-agenda-structural-header' text property; everything
;; between one header and the next that carries `org-marker' is an entry.
(defun shan/org-agenda-append-counts ()
  "Append [n] to each agenda section header, counting its entries."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (let (header-pos (count 0))
        (cl-flet ((flush ()
                    (when (and header-pos (> count 0))
                      (save-excursion
                        (goto-char header-pos)
                        (end-of-line)
                        (insert (propertize (format " [%d]" count)
                                            'face 'org-agenda-structure))))))
          (while (not (eobp))
            (cond
             ((get-text-property (point) 'org-agenda-structural-header)
              (flush)
              (setq header-pos (point) count 0))
             ((get-text-property (point) 'org-marker)
              (setq count (1+ count))))
            (forward-line 1))
          (flush))))))
(add-hook 'org-agenda-finalize-hook #'shan/org-agenda-append-counts)

;; --- Quick access to org files -----------------------------------------------
(defun shan/org-open-notes ()
  "Open notes.org in `org-directory'."
  (interactive)
  (find-file (expand-file-name "notes.org" org-directory)))

(defun shan/org-open-inbox ()
  "Open inbox.org in `org-directory'."
  (interactive)
  (find-file (expand-file-name "inbox.org" org-directory)))

(defun shan/org-refresh-github-issues ()
  "Regenerate github.org from issues assigned to you on GitHub.
Runs the gh-issues-to-org script (dotfiles/bin) asynchronously; the file
is rewritten wholesale, so anything typed into it is lost -- keep notes in
inbox.org with a link back instead."
  (interactive)
  (let ((script (expand-file-name "~/.local/bin/gh-issues-to-org")))
    (if (not (file-executable-p script))
        (user-error "Not found: %s" script)
      (message "Fetching GitHub issues...")
      (set-process-sentinel
       (start-process "gh-issues-to-org" "*gh-issues*" script)
       (lambda (_p event)
         (if (string-match-p "finished" event)
             (progn
               (dolist (b (buffer-list))
                 (when (and (buffer-file-name b)
                            (string-suffix-p "github.org" (buffer-file-name b)))
                   (with-current-buffer b (revert-buffer t t t))))
               (message "GitHub issues refreshed"))
           (message "gh-issues-to-org failed; see *gh-issues*")))))))

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
