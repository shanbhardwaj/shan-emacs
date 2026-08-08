;;; init-vc.el --- Version control configuration -*- lexical-binding: t; -*-
;;; Commentary:

(use-package vc
  :ensure nil                        ;; This is built-in, no need to fetch it.
  :defer t
  :bind
  (("C-x v d" . vc-dir)              ;; Open VC directory for version control status.
   ("C-x v =" . vc-diff)             ;; Show differences for the current file.
   ("C-x v D" . vc-root-diff)        ;; Show differences for the entire repository.
   ("C-x v v" . vc-next-action))     ;; Perform the next version control action.
  :config
  ;; Better colors for <leader> g b  (blame file) 
  (setq vc-annotate-color-map
        '((20 . "#f5e0dc")
          (40 . "#f2cdcd")
          (60 . "#f5c2e7")
          (80 . "#cba6f7")
          (100 . "#f38ba8")
          (120 . "#eba0ac")
          (140 . "#fab387")
          (160 . "#f9e2af")
          (180 . "#a6e3a1")
          (200 . "#94e2d5")
          (220 . "#89dceb")
          (240 . "#74c7ec")
          (260 . "#89b4fa")
          (280 . "#b4befe"))))

(use-package smerge-mode
  :ensure nil                                  ;; This is built-in, no need to fetch it.
  :defer t
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)  ;; Keep the changes from the upper version.
              ("C-c ^ l" . smerge-keep-lower)  ;; Keep the changes from the lower version.
              ("C-c ^ n" . smerge-next)        ;; Move to the next conflict.
              ("C-c ^ p" . smerge-previous)))  ;; Move to the previous conflict.

;; --- PR review helpers ------------------------------------------------------
;; Give diff/revision/log buffers the WHOLE frame instead of the cramped
;; side-popup that `same-window-except-diff' produced.  `q' restores the
;; previous window layout, so review stays a "fly through" loop.
(defun shan/magit-display-buffer (buffer)
  "Display magit BUFFER: diffs/revisions/logs full-frame, everything else same-window."
  (if (with-current-buffer buffer
        (derived-mode-p 'magit-diff-mode 'magit-revision-mode 'magit-log-mode))
      (display-buffer buffer '(display-buffer-full-frame))
    (magit-display-buffer-same-window-except-diff-v1 buffer)))

(defvar shan/pr-base-branch "docs/new-schema-design"
  "Base branch PRs target in this repo; used by `shan/review-pr'.")

(defun shan/review-pr (pr)
  "Review GitHub PR number PR (or a branch name): fetch it and show the range
diff against `shan/pr-base-branch', filling the frame.  Review-only — never
checks out or merges.  Requires the `gh' CLI to be authed."
  (interactive "sPR number or branch: ")
  (let* ((default-directory (or (magit-toplevel) default-directory))
         (base shan/pr-base-branch)
         (branch (if (string-match-p "\\`[0-9]+\\'" pr)
                     (string-trim
                      (shell-command-to-string
                       (format "gh pr view %s --json headRefName --jq .headRefName"
                               (shell-quote-argument pr))))
                   pr)))
    (when (string-empty-p branch)
      (user-error "Could not resolve PR/branch %S (is `gh' authed?)" pr))
    (message "Fetching origin/%s and origin/%s…" base branch)
    (magit-run-git "fetch" "origin" base branch)
    (magit-diff-range (format "origin/%s...origin/%s" base branch))))

;; Bound directly (not via magit's :bind) since it is not a magit command.
;; Under the C-c v (dev-workflow) prefix — C-c p is taken by cape.
(global-set-key (kbd "C-c v r") #'shan/review-pr)

(use-package magit
  :ensure t
  :commands (magit-status magit-ediff-show-working-tree)
  :bind (("C-x g" . magit-status)        ; conventional magit entry point
         ("C-c C-d" . magit-ediff-show-working-tree))
  :custom
  (magit-display-buffer-function #'shan/magit-display-buffer)
  (magit-diff-refine-hunk 'all))          ; word-level highlighting within hunks

(use-package blamer
  :ensure t
  :commands (blamer-mode)
  :config
  (setq blamer-view 'overlay-right
        blamer-type 'visual
        blamer-max-commit-message-length 70
        blamer-force-truncate-long-line nil
        blamer-author-formatter " ✎ %s "
        blamer-commit-formatter "● \'%s\' ● ")
  :custom
  (blamer-idle-time 1.0)
  :custom-face
  (blamer-face ((t :foreground "#E46876"
                   :height 130
                   :bold t
                   :italic t))))

(use-package forge
  :ensure t
  :after magit
  :defer t
  :config
  (defun shan/forge-my-issues ()
    "List my open issues in the current repository's forge."
    (interactive)
    (forge-topics-setup-buffer nil nil
                               :type 'issue
                               :assignee (magit-get "github.user")
                               :state 'open))
  (bind-key "H-i" 'shan/forge-my-issues))

(use-package orgit-forge
  :ensure t
  :after forge)

(use-package git-timemachine
  :ensure t
  :defer t
  )

(use-package git-link
  :ensure t
  :defer t)

(use-package git-gutter
  :ensure t
  :defer t
  :hook (prog-mode . git-gutter-mode)
  :diminish git-gutter-mode
  :config
  (setq git-gutter:update-interval 1))

(use-package git-gutter-fringe
  :ensure t
  :after git-gutter
  :config
  (setq git-gutter-fr:side 'left-fringe)
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated)))
