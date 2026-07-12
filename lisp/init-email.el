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
                                 (or (when (string-match "\\`/\\([^/]+\\)" md)
                                       (cdr (assoc (match-string 1 md)
                                                   '(("addval"    . "shan@addvalsolutions.com")
                                                     ("kulcare"   . "shantanu@kulcare.com")
                                                     ("codetiger" . "shan@codetiger.com")
                                                     ("gmail"     . "bhardwaj.10@gmail.com")))))
                                     md))))))

  ;; hide deleted-but-not-yet-expunged messages from all headers views
  (setq mu4e-search-hide-predicate
        (lambda (msg)
          (memq 'trashed (mu4e-message-field msg :flags))))

  (setq mu4e-maildir-shortcuts
        '(("/kulcare/INBOX"             . ?k)
          ("/addval/INBOX"              . ?a)
          ("/gmail/INBOX"               . ?g)
          ("/codetiger/INBOX"           . ?c)
          ("/kulcare/[Gmail]/Sent Mail" . ?s)
          ("/kulcare/[Gmail]/Trash"     . ?t)
          ("/kulcare/[Gmail]/Drafts"    . ?d)
          ("/kulcare/[Gmail]/All Mail"  . ?l)))

  (setq mu4e-bookmarks
        '((:name "Inbox - Kulcare"  :query "maildir:/kulcare/INBOX"  :key ?k)
          (:name "Inbox - Addval"   :query "maildir:/addval/INBOX"   :key ?a)
          (:name "Inbox - Gmail"    :query "maildir:/gmail/INBOX"    :key ?g)
          (:name "Inbox - Codetiger":query "maildir:/codetiger/INBOX":key ?c)))

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
                    (mu4e-trash-folder     . "/gmail/[Gmail]/Trash"))))))

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :config
  (setq mu4e-alert-email-notification-types '(count))
  (mu4e-alert-enable-mode-line-display))

(use-package mu4e-column-faces
  :after mu4e
  :config (mu4e-column-faces-mode))

(use-package mu4e-views
  :after mu4e
  :init
  ;; mu4e-views uses goto-address-url-regexp at load time without
  ;; requiring goto-addr (upstream bug), so load it first
  (require 'goto-addr))
