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
         ("C-c o G" . shan/org-refresh-github-issues)
         ("C-c o p" . shan/org-open-project)
         ("C-c o w" . shan/org-agenda-for-person)
         ("C-c o d" . shan/org-day-plan)
         ("C-c o r" . shan/org-day-close)
         ("C-c o t" . shan/org-today-toggle)
         ("C-c o D" . shan/org-today-clear))
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
      (;; What you committed to this morning, above everything else.
       (tags "today"
             ((org-agenda-overriding-header "Today")))
       (agenda "" ((org-agenda-span 'day)
                   (org-deadline-warning-days 7)))
       (todo "NEXT"
             ((org-agenda-overriding-header "In progress")))
       ;; -github keeps the 50-odd issues in github.org out of here; they
       ;; get their own section below.  Without it they drown the inbox.
       ;; -learning-reading keep the Emacs curriculum and the saved links out:
       ;; both are piles you visit deliberately, not things to see every day.
       (tags-todo "-SCHEDULED={.}-DEADLINE={.}-github-learning-reading/!TODO"
                  ((org-agenda-overriding-header "Inbox (no date)")
                   (org-agenda-sorting-strategy '(timestamp-down))))
       (tags-todo "+github/!TODO"
                  ((org-agenda-overriding-header "GitHub (assigned to me)")
                   (org-agenda-max-entries 12)))
       (todo "WAITING"
             ((org-agenda-overriding-header "Waiting on someone")))))

     ;; Morning.  Top block is what is already committed (usually empty, or
     ;; whatever carried over); the rest is the pool to choose from, grouped
     ;; the way you actually think about it -- questions first, since that is
     ;; most of what gets captured.  Press `t' on a line to commit it.
     ("P" "Plan the day"
      ((tags "today"
             ((org-agenda-overriding-header "Committed")))
       (agenda "" ((org-agenda-span 'day)
                   (org-agenda-overriding-header "Today's calendar")))
       (todo "Q|ASKED"
             ((org-agenda-overriding-header "Open questions")))
       (tags-todo "-github-learning-reading/!TODO|NEXT"
                  ((org-agenda-overriding-header "Everything else open")
                   (org-agenda-sorting-strategy '(priority-down timestamp-down))))))

     ;; Evening.  One block: what you said you would do.  Mark what got done,
     ;; then clear the tags with `C-c o D' so tomorrow starts empty.
     ("R" "Close the day"
      ((tags "today"
             ((org-agenda-overriding-header
               "Today's commitments -- mark done, then C-c o D to clear")))))))

  ;; Tasks
  ;; Two sequences, deliberately.  Most of what gets captured for a project is
  ;; an open question, and a question does not have the life of a task: it is
  ;; raised, it waits on a person or a decision, then it is answered -- and the
  ;; answer is sometimes worth keeping long after the item closes.  Forcing
  ;; that through TODO/NEXT/DONE made questions read as stalled work.
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
     (sequence "Q(q)" "ASKED(a)" "|" "ANSWERED(A)" "MOOT(m)")))
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
      "* TODO %?\n%U\n[[%:link][%:description]]\n\n%i" :empty-lines 1)

     ;; Project captures.  These prompt for the project and route straight to
     ;; the right file, so nothing accumulates a refiling debt.  %^g prompts
     ;; for tags with completion over tags already in use, which is where the
     ;; person goes -- :@ravi: and the like.  The project tag is supplied by
     ;; the file's #+FILETAGS, so it never has to be typed.
     ;; Nested under p, so the letters match everywhere they are typed:
     ;;   Emacs      C-c o c  p q
     ;;   Mac        Leader   k p q
     ;;   4090       Alt+Ctrl+Shift+Q   (no namespace there, so Shift = project)
     ("p" "Project")
     ("pq" "Question" entry
      (function (lambda () (shan/org-capture-project-target "Open questions")))
      "* Q %^{Question} %^g\n%U\n%?" :empty-lines 1)
     ("pt" "Task" entry
      (function (lambda () (shan/org-capture-project-target "Tasks")))
      "* TODO %^{Task} %^g\n%U\n%?" :empty-lines 1)
     ("pn" "Note" entry
      (function (lambda () (shan/org-capture-project-target "Notes")))
      "* %^{Note} %^g\n%U\n%?" :empty-lines 1)))

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

;; --- The daily loop ----------------------------------------------------------
;; Commitment is a TAG, not a date and not a keyword.
;;
;; A tag is orthogonal to state, which matters here because most of what gets
;; captured is an open question rather than a task -- a Q and a TODO can both
;; be today's work.  It carries no date semantics, so nothing silently becomes
;; "overdue" and breeds the guilt that kills these systems.  And it can be
;; wiped in one pass at the end of the day, which is what makes the loop
;; closable rather than cumulative.
;;
;; SCHEDULED was the obvious alternative and is worse: it means "start on this
;; date", so a day you do not finish turns into a growing overdue list.  NEXT
;; keeps its real meaning -- actively in progress right now.
;;
;;   C-c o d   morning: what carried over, then the pool to pick from
;;   C-c o t   commit the entry at point (or un-commit it)
;;   C-c o r   evening: what you committed to
;;   C-c o D   clear every tag, so tomorrow starts empty

(defcustom shan/org-today-tag "today"
  "Tag marking an entry as committed to for today."
  :type 'string :group 'org)

(defun shan/org-today-toggle ()
  "Commit the entry at point to today, or take it back.
Works in a file and in the agenda."
  (interactive)
  (if (derived-mode-p 'org-agenda-mode)
      (let ((m (or (org-get-at-bol 'org-hd-marker)
                   (org-get-at-bol 'org-marker))))
        (unless m (user-error "No entry at point"))
        (org-with-point-at m
          (org-toggle-tag shan/org-today-tag 'toggle))
        (org-agenda-redo t))
    (org-toggle-tag shan/org-today-tag 'toggle)))

(defun shan/org-day-plan ()
  "Morning pass: see what carried over, then choose today's work."
  (interactive)
  (org-agenda nil "P"))

(defun shan/org-day-close ()
  "Evening pass: what you committed to.  Clear with `shan/org-today-clear'."
  (interactive)
  (org-agenda nil "R"))

(defun shan/org-today-clear ()
  "Remove the today tag from every entry that carries it.
Only local tags are touched, never inherited ones, so a file-wide
#+FILETAGS is never damaged.  Run at the end of the day: carrying a
commitment over should be a deliberate act tomorrow, not the default."
  (interactive)
  (let ((n 0) (files 0))
    (dolist (file (org-agenda-files))
      (let* ((buf (find-file-noselect file))
             (touched nil))
        (with-current-buffer buf
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward org-heading-regexp nil t)
             (when (member shan/org-today-tag (org-get-tags nil t))
               (org-toggle-tag shan/org-today-tag 'off)
               (setq touched t n (1+ n))))))
        (when touched
          (setq files (1+ files))
          (with-current-buffer buf (save-buffer)))))
    (message "Cleared %s from %d entr%s in %d file%s"
             shan/org-today-tag n (if (= n 1) "y" "ies")
             files (if (= files 1) "" "s"))))

;; --- Projects ----------------------------------------------------------------
;; Two axes, not one.  An item belongs to a project AND usually to a person --
;; the person you need to raise it with.  A directory can only express one of
;; those, so the project is a file (because you read by sitting down on one
;; project) and the person is a tag (because reviews are irregular and
;; person-shaped, and the same people work across several projects).
;;
;; #+FILETAGS in each project file supplies the project tag automatically, so
;; the only tag ever typed is the person.  One item, both views, filed once.

(defvar shan/org-projects '("kulcare" "commander" "finishd" "trekka")
  "Active projects.  Each has a <name>.org in `org-directory'.
Rituality was completed and handed over, so it has no file here; its
history stays in github.org.")

(defun shan/org-project-file (project)
  "Return the org file for PROJECT."
  (expand-file-name (concat project ".org") org-directory))

(defun shan/org-capture-project-target (heading)
  "Prompt for a project, then put point at the end of HEADING in its file.
Used as a `function' capture target.  Creates HEADING if it is missing, so
a hand-edited project file cannot break capture."
  (let* ((project (completing-read "Project: " shan/org-projects nil t))
         (file (shan/org-project-file project)))
    (set-buffer (org-capture-target-buffer file))
    (widen)
    (goto-char (point-min))
    (unless (re-search-forward (concat "^\\* " (regexp-quote heading) "[ \t]*$") nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " heading "\n")
      (forward-line -1))
    (end-of-line)))

(defun shan/org-open-project ()
  "Open one of `shan/org-projects'."
  (interactive)
  (find-file (shan/org-project-file
              (completing-read "Project: " shan/org-projects nil t))))

(defun shan/org-person-tags ()
  "Every tag starting with @ that is already in use across the agenda files."
  (let (tags)
    (dolist (entry (org-global-tags-completion-table (org-agenda-files)))
      (let ((tag (if (consp entry) (car entry) entry)))
        (when (and (stringp tag) (string-prefix-p "@" tag))
          (push tag tags))))
    (sort (delete-dups tags) #'string<)))

(defun shan/org-agenda-for-person ()
  "Show every open item tagged for one person, across all projects.
This is the review view: irregular meetings mean the useful question is
never \"what is scheduled\" but \"what have I got for this person\"."
  (interactive)
  (let ((who (completing-read "Person tag: " (shan/org-person-tags) nil nil "@")))
    (org-tags-view nil (concat who "/!"))))

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
