;;; init-settings.el --- General settings -*- lexical-binding: t; -*-
(use-package emacs
  :ensure nil
  :custom
  (column-number-mode t)
  ;; backups and auto-saves on, tucked away under var/ by
  ;; no-littering-theme-backups (see init.el)
  (auto-save-default t)
  (make-backup-files t)
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (create-lockfiles nil)
  (delete-selection-mode 1)
  (display-line-numbers-type 'relative)
  (global-auto-revert-non-file-buffers t)
  (history-length 500)
  (ispell-dictionary "en_US")
  (pixel-scroll-precision-use-momentum nil)
  (split-width-threshold 300)
  (switch-to-buffer-obey-display-actions t)
  (tab-always-indent 'complete)
  (tab-width 2)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (use-dialog-box nil)
  (use-short-answers t)
  (uniquify-buffer-name-style 'forward)
  (help-window-select t)

  :config
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  (setq process-adaptive-read-buffering nil)
  (setq read-process-output-max (* 4 1024 1024))

  :init
  (global-auto-revert-mode 1)
  (save-place-mode 1)
  (winner-mode 1)
  (file-name-shadow-mode 1)
  (run-at-time nil 600 'recentf-save-list))

(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1)))))

(setq sentence-end-double-space nil)
;; plain -c: with bash, -ic would spew "cannot set terminal process
;; group" warnings into every M-x shell-command (fish tolerated -ic)
(setq shell-command-switch "-c")

;; --- TRAMP (remote editing over ssh) ---------------------------------
;; ssh connection sharing comes from ~/.ssh/config (ControlPersist);
;; auto-saves and backups of remote files stay local via no-littering
(setq tramp-default-method "ssh")
(setq tramp-verbose 1)                  ; errors only; default 3 is chatty
(setq remote-file-name-inhibit-locks t) ; don't scatter .#lockfiles remotely

;; ssh keepalive: only helps when the *host* dies (network drop) -- ssh
;; then notices the dead peer in ~10s and errors out instead of blocking
;; accept-process-output forever. It does NOT stop the 100% CPU livelock
;; below (that host stays up and answers keepalives). Tramp injects these
;; itself (tramp-use-ssh-controlmaster-options defaults to t) and ignores
;; ~/.ssh/config's ControlMaster, so they must go here, not in ssh config.
;; Keep %%C literal -- Tramp %-expands this string.
(setq tramp-ssh-controlmaster-options
      (concat "-o ControlMaster=auto "
              "-o ControlPath='tramp.%%C' "
              "-o ControlPersist=no "
              "-o ServerAliveInterval=5 "
              "-o ServerAliveCountMax=2"))
(setq tramp-connection-timeout 10)      ; fail fast on the initial handshake

;; THE actual daemon-livelock fix. Symptom: Emacs pegged at 100% CPU,
;; unresponsive, sample shows one endless stack:
;;   redisplay -> doom-modeline file-truename on a /ssh: buffer
;;     -> tramp-wait-for-regexp, whose loop calls (sit-for 0 'nodisp)
;;     -> sit-for runs due timers -> global-auto-revert's timer fires
;;     -> auto-revert--buffer-candidates calls (file-remote-p ... t)
;;     -> re-enters Tramp mid-command  ==> spin.
;; Tramp is not reentrant; the timer re-entry is what turns a quiet block
;; into a 100% CPU spin. auto-revert-remote-files/global-auto-revert-ignore-
;; buffer don't help (the candidate scan re-enters before either is checked).
;; Fix: drop remote buffers from the poll set entirely by clearing the
;; buffer-local flag auto-revert--polled-buffers keys on. Mode line and
;; local-buffer auto-revert are unaffected.
(defun shan/no-autorevert-remote ()
  "Exclude remote (Tramp) buffers from `global-auto-revert-mode' polling."
  (when (and buffer-file-name (file-remote-p buffer-file-name))
    (setq-local auto-revert--global-mode nil)))
(add-hook 'find-file-hook #'shan/no-autorevert-remote)

;; save-place's exit-time cleanup runs a bare file-readable-p over every
;; saved entry (no remote guard) from kill-emacs-hook -- with a dead
;; remote that hangs Emacs on exit. Skip the cleanup; stale entries are
;; harmless.
(setq save-place-forget-unreadable-files nil)

;; git-gutter has no remote awareness: its 1s update timer would run git
;; over Tramp for every remote prog-mode buffer, and a dropped connection
;; wedges it. Keep it local-only.
(with-eval-after-load 'git-gutter
  (advice-add 'git-gutter-mode :before-while
              (lambda (&optional _arg)
                (not (file-remote-p default-directory)))))

;; Don't run vc's automatic backend detection on remote files when
;; visiting them (per the Tramp manual's own recommendation); magit and
;; explicit vc commands still work when invoked.
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))

;; save bookmarks immediately, not just at exit
(setq bookmark-save-flag 1)

;; M-x shell on remote hosts: use bash, no "Remote shell path" prompt
;; (with vertico, RET in that prompt submits the directory -> env error)
(connection-local-set-profile-variables
 'shan/remote-bash
 '((explicit-shell-file-name . "/bin/bash")
   (shell-file-name . "/bin/bash")))
(connection-local-set-profiles
 '(:application tramp) 'shan/remote-bash)

(global-unset-key (kbd "M-m"))
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(bind-key "<s-return>" 'toggle-frame-fullscreen)
(bind-key "s-[" 'previous-buffer)
(bind-key "s-]" 'next-buffer)
(bind-key "M-C-w" 'restart-emacs)
(bind-key "M-o" 'other-window)
(bind-key "H-c" 'compile)
(bind-key "H-r" 'recompile)
;; function-key-free alternatives (no dedicated F-keys on this keyboard)
(bind-key "H-n" 'shan/neotree-project-toggle) ; also on C-<f8>
(bind-key "H-t" 'consult-theme)               ; also on s-<f12>
;; hyper launcher layer: single-chord versions of daily commands
(bind-key "H-m" 'mu4e)                        ; also on C-c m
(bind-key "H-d" 'mu4e-dashboard)              ; also on C-c M
(bind-key "H-p" 'project-switch-project)      ; also on C-x p p
(bind-key "H-f" 'consult-project-extra-find)  ; also on C-c p F
(bind-key "H-b" 'consult-buffer)              ; also on C-x b
(bind-key "H-g" 'ghostel-project)             ; also on C-x p t
(bind-key "H-," (defun shan/open-init () (interactive) (find-file "~/.emacs.d/init.el")))
(bind-key "H-s" (defun save-and-recompile () (interactive) (save-buffer) (recompile)))

(windmove-default-keybindings 'super)
(bind-key "M-[" 'windmove-left)
(bind-key "M-]" 'windmove-right)

(defun align-values (start end)
  "Vertically aligns region based on lengths of the first value of each line."
  (interactive "r")
  (align-regexp start end
                "\\([a-z_]+: \\)"
                -1 1 nil))
