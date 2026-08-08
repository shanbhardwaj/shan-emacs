;;; init-email.el --- Email configuration -*- lexical-binding: t; -*-

(use-package mu4e
  :load-path
  (lambda ()
    (expand-file-name "share/emacs/site-lisp/mu/mu4e/"
                      (file-name-directory
                       (directory-file-name
                        (file-name-directory (executable-find "mu"))))))
  :custom
  (mu4e-use-fancy-chars nil)
  (mu4e-mu-binary (executable-find "mu"))
  ;; mail fetching is handled externally by the homebrew.mxcl.isync
  ;; launchd service (mbsync -a every 300s); mu4e only reindexes here
  (mu4e-update-interval 300)
  (mu4e-maildir "~/Mail")
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-change-filenames-when-moving t)
  (mu4e-search-include-related nil)
  (mu4e-headers-visible-lines 25)
  ;; HTML mail rendering (shr)
  (shr-use-colors nil)
  (shr-width 80)
  (shr-max-image-proportion 0.6)
  (gnus-unbuttonized-mime-types '("text/plain" "text/html"))
  (mu4e-compose-format-flowed t)
  (mu4e-compose-context-policy 'ask-if-none)
  (message-kill-buffer-on-exit t)
  (send-mail-function 'sendmail-send-it)
  (message-send-mail-function 'sendmail-send-it)
  (sendmail-program (executable-find "msmtp"))
  (message-sendmail-envelope-from 'header)

  :hook
  ((mu4e-view-mode . (lambda ()
                       (visual-line-mode 1)
                       (setq-local line-spacing 0.2)))
   (mu4e-compose-mode . (lambda ()
                          (use-hard-newlines -1)
                          (flyspell-mode)))
   (mu4e-headers-mode . (lambda ()
                          (interactive)
                          (setq mu4e-headers-fields
                                `((:human-date . 25)
                                  (:flags . 6)
                                  (:account . 10)
                                  (:from . 25)
                                  (:thread-subject . ,(- (window-body-width) 86))
                                  (:size . 7))))))

  :bind
  (("C-c m" . mu4e)
   (:map mu4e-search-minor-mode-map
         ("S" . mu4e-kill-update-mail)))

  :config
  ;; custom headers column: the address of the account a mail arrived at
  (add-to-list 'mu4e-header-info-custom
               '(:account . (:name "Account"
                             :shortname "Acct"
                             :help "Address the message was delivered to"
                             :function
                             (lambda (msg)
                               (let ((md (mu4e-message-field msg :maildir)))
                                 (if (string-match "\\`/\\([^/]+\\)" md)
                                     (capitalize (match-string 1 md))
                                   md))))))

  ;; hide trashed/spam mail from search results: both messages marked for
  ;; deletion (trashed flag) and mail living in Trash/Spam folders (e.g.
  ;; deleted from another device). Skipped when the query itself asks for
  ;; Trash/Spam, so those folders stay browsable via j or s.
  (setq mu4e-search-hide-predicate
        (lambda (msg)
          (let ((md (mu4e-message-field msg :maildir))
                (query (or (mu4e-last-query) "")))
            (and (not (string-match-p "Trash\\|Spam" query))
                 (or (memq 'trashed (mu4e-message-field msg :flags))
                     (string-match-p "/\\(Trash\\|Spam\\)\\'" md))))))

  ;; ^ jumps straight back to the main screen from headers or the
  ;; message view, collapsing the split (dired-style "up")
  (defun shan/mu4e-goto-main ()
    "Close the view/headers buffers and return to the mu4e main screen."
    (interactive)
    (when (derived-mode-p 'mu4e-view-mode)
      (mu4e-view-quit))
    (when (derived-mode-p 'mu4e-headers-mode)
      (mu4e~headers-quit-buffer)))
  (define-key mu4e-headers-mode-map (kbd "^") #'shan/mu4e-goto-main)
  (define-key mu4e-view-mode-map (kbd "^") #'shan/mu4e-goto-main)

  (setq mu4e-maildir-shortcuts
        '(("/kulcare/INBOX"             . ?k)
          ("/addval/INBOX"              . ?a)
          ("/gmail/INBOX"               . ?g)
          ("/codetiger/INBOX"           . ?c)
          ("/kulcare/[Gmail]/Sent Mail" . ?s)
          ("/kulcare/[Gmail]/Trash"     . ?t)
          ("/kulcare/[Gmail]/Drafts"    . ?d)
          ("/kulcare/[Gmail]/All Mail"  . ?l)))

  ;; every entry on the main screen is a bookmark so that mu4e keeps its
  ;; count/unread numbers up to date (via `mu4e-query-items')
  (setq mu4e-bookmarks
        (let ((inboxes (concat "maildir:/kulcare/INBOX OR maildir:/addval/INBOX "
                               "OR maildir:/gmail/INBOX OR maildir:/codetiger/INBOX"))
              (sent (mapconcat (lambda (a)
                                 (format "maildir:\"/%s/[Gmail]/Sent Mail\"" a))
                               '("kulcare" "addval" "gmail" "codetiger")
                               " OR ")))
          `((:name "Unread" :query ,(format "(%s) AND flag:unread" inboxes)
             :key ?u :favorite t) ; favorite: unread count shows in the modeline
            (:name "Inbox"      :query ,inboxes             :key ?i)
            (:name "Reply queue" :query ,(format "(%s) AND flag:flagged" inboxes) :key ?f)
            (:name "Drafts"     :query "flag:draft"         :key ?d)
            (:name "Sent"       :query ,sent                :key ?s)
            (:name "Today"      :query "date:today..now"    :key ?t)
            (:name "Yesterday"  :query "date:2d..1d"        :key ?y)
            (:name "Last week"  :query "date:7d..now"       :key ?w)
            (:name "Last month" :query "date:4w..now"       :key ?m)
            (:name "Kulcare"    :query "maildir:/kulcare/INBOX"   :key ?k)
            (:name "Addval"     :query "maildir:/addval/INBOX"    :key ?a)
            (:name "Gmail"      :query "maildir:/gmail/INBOX"     :key ?g)
            (:name "Codetiger"  :query "maildir:/codetiger/INBOX" :key ?c))))

  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Addval"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/addval" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address     . "shan@addvalsolutions.com")
                    (user-full-name        . "Shantanu Bhardwaj")
                    (mu4e-drafts-folder    . "/addval/[Gmail]/Drafts")
                    (mu4e-sent-folder      . "/addval/[Gmail]/Sent Mail")
                    (mu4e-refile-folder    . "/addval/[Gmail]/All Mail")
                    (mu4e-trash-folder     . "/addval/[Gmail]/Trash")))

          ,(make-mu4e-context
            :name "Kulcare"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/kulcare" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address     . "shantanu@kulcare.com")
                    (user-full-name        . "Shantanu Bhardwaj")
                    (mu4e-drafts-folder    . "/kulcare/[Gmail]/Drafts")
                    (mu4e-sent-folder      . "/kulcare/[Gmail]/Sent Mail")
                    (mu4e-refile-folder    . "/kulcare/[Gmail]/All Mail")
                    (mu4e-trash-folder     . "/kulcare/[Gmail]/Trash")))

          ,(make-mu4e-context
            :name "Codetiger"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/codetiger" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address     . "shan@codetiger.com")
                    (user-full-name        . "Shan Bhardwaj")
                    (mu4e-drafts-folder    . "/codetiger/[Gmail]/Drafts")
                    (mu4e-sent-folder      . "/codetiger/[Gmail]/Sent Mail")
                    (mu4e-refile-folder    . "/codetiger/[Gmail]/All Mail")
                    (mu4e-trash-folder     . "/codetiger/[Gmail]/Trash")))

          ,(make-mu4e-context
            :name "Gmail"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address     . "bhardwaj.10@gmail.com")
                    (user-full-name        . "Shantanu Bhardwaj")
                    (mu4e-drafts-folder    . "/gmail/[Gmail]/Drafts")
                    (mu4e-sent-folder      . "/gmail/[Gmail]/Sent Mail")
                    (mu4e-refile-folder    . "/gmail/[Gmail]/All Mail")
                    (mu4e-trash-folder     . "/gmail/[Gmail]/Trash")))))

  ;; --- Rougier-style main screen -------------------------------------
  ;; Replaces the default mu4e main view with a compact dashboard; all
  ;; counts come from `mu4e-query-items' so they refresh with the index.

  (defun shan/mu4e--counts (key)
    "Query-item plist (with :count/:unread) for the bookmark with char KEY."
    (or (seq-find (lambda (item) (eq (plist-get item :key) key))
                  (mu4e-query-items 'bookmarks))
        '(:count 0 :unread 0)))

  (defun shan/mu4e--bookmark-cmd (key)
    "Command running the search of the bookmark with char KEY."
    (lambda ()
      (interactive)
      (mu4e-search-bookmark (mu4e-get-bookmark-query key))))

  (defun shan/mu4e--main-cell (label key cmd width &optional count)
    "Dot-padded cell \"LABEL (COUNT) ... [KEY]\" of WIDTH chars, activating CMD."
    (let* ((text (if count (format "%s (%d)" label count) label))
           (dots (max 1 (- width (string-width text) 5)))
           (map (make-sparse-keymap)))
      (define-key map [mouse-1] cmd)
      (define-key map (kbd "RET") cmd)
      (concat
       (propertize text 'face 'mu4e-highlight-face
                   'mouse-face 'highlight 'keymap map)
       " " (make-string dots ?.) " "
       (propertize (format "[%s]" (string key)) 'face 'shadow))))

  (defun shan/mu4e--main-contents ()
    "Render the Rougier-style main screen."
    (let* ((w1 24) (w2 27) (w3 20) (sep "   ")
           (line-width (+ 2 w1 w2 w3 (* 2 (length sep))))
           (col1 '(("Unread" ?u) ("Inbox" ?i) ("Drafts" ?d) ("Sent" ?s)))
           (col2 '(("Today" ?t) ("Yesterday" ?y)
                   ("Last week" ?w) ("Last month" ?m)))
           (col3 (list (list "Compose" ?C #'mu4e-compose-new)
                       (list "Update" ?U #'mu4e-update-mail-and-index)
                       (list "Switch context" ?\; #'mu4e-context-switch)
                       (list "Quit" ?q #'mu4e-quit)))
           (header
            (let ((left "* Mu for Emacs (mu4e)")
                  (right (format "%d unread"
                                 (or (plist-get (shan/mu4e--counts ?u) :count)
                                     0))))
              (concat
               (propertize left 'face 'mu4e-title-face)
               (make-string (max 1 (- line-width (length left) (length right)))
                            ?\s)
               (propertize right 'face 'mu4e-header-key-face))))
           (bookmark-cell
            (lambda (entry width &optional unread)
              (pcase-let ((`(,label ,key) entry))
                (shan/mu4e--main-cell
                 label key (shan/mu4e--bookmark-cmd key) width
                 (or (plist-get (shan/mu4e--counts key)
                                (if unread :unread :count))
                     0))))))
      (concat
       header "\n\n"
       (mapconcat
        (lambda (i)
          (pcase-let ((`(,label ,key ,cmd) (nth i col3)))
            (concat
             "  "
             (funcall bookmark-cell (nth i col1) w1) sep
             (funcall bookmark-cell (nth i col2) w2) sep
             (shan/mu4e--main-cell label key cmd w3))))
        '(0 1 2 3) "\n")
       "\n\n"
       ;; per-account inboxes, showing unread counts
       "  " (funcall bookmark-cell '("Kulcare" ?k) w1 'unread) sep
       (funcall bookmark-cell '("Addval" ?a) w2 'unread) "\n"
       "  " (funcall bookmark-cell '("Gmail" ?g) w1 'unread) sep
       (funcall bookmark-cell '("Codetiger" ?c) w2 'unread) "\n")))

  (defun shan/mu4e--main-redraw ()
    "Redraw the mu4e main buffer, Rougier style."
    (when-let* ((buffer (get-buffer mu4e-main-buffer-name))
                (buffer (and (buffer-live-p buffer) buffer)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (pos (point)))
          (unless (eq major-mode 'mu4e-main-mode)
            (mu4e-main-mode))
          (erase-buffer)
          (insert (shan/mu4e--main-contents))
          (goto-char (min pos (point-max)))))))

  (advice-add #'mu4e--main-redraw :override #'shan/mu4e--main-redraw)

  (defun shan/mu4e-main-setup-keys ()
    "Bind the dashboard's single-key searches in the main buffer only."
    (let ((map (make-sparse-keymap)))
      ;; inherit search-minor-mode bindings, then shadow ours on top;
      ;; plain search moves from `s' (now Sent) to `/'
      (set-keymap-parent map mu4e-search-minor-mode-map)
      (dolist (key '(?u ?i ?d ?s ?t ?y ?w ?m ?k ?a ?g ?c))
        (define-key map (string key) (shan/mu4e--bookmark-cmd key)))
      (define-key map "/" #'mu4e-search)
      (setq-local minor-mode-overriding-map-alist
                  (list (cons 'mu4e-search-minor-mode map)))))
  (add-hook 'mu4e-main-mode-hook #'shan/mu4e-main-setup-keys))

;; unread count in the modeline comes from mu4e's native mu4e-modeline-mode
;; via the :favorite bookmark (mu4e-alert was removed as redundant)

(use-package mu4e-column-faces
  :ensure t
  :after mu4e
  :config (mu4e-column-faces-mode))

(use-package mu4e-views
  :ensure t
  :after mu4e
  :init
  ;; mu4e-views uses goto-address-url-regexp at load time without
  ;; requiring goto-addr (upstream bug), so load it first
  (require 'goto-addr))
