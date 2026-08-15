;;; init-project.el --- project.el configuration -*- lexical-binding: t; -*-

;; Built-in project.el is the single source of project awareness:
;; claude-code-ide resolves its session root through it, ghostel provides
;; project-scoped terminals, consult-project-extra reads its project list,
;; and speedbar provides the file tree below.

(use-package project
  :ensure nil
  :custom
  ;; also treat non-git directories containing a .project file as roots
  ;; (add markers like "package.json" here if monorepo subpackages should
  ;; count as their own projects)
  (project-vc-extra-root-markers '(".project"))
  :config
  ;; menu shown by C-x p p after picking a project
  (setq project-switch-commands
        '((project-find-file        "Find file"  ?f)
          (consult-project-extra-find "Consult"  ?F)
          (project-find-regexp      "Grep"       ?g)
          (project-dired            "Dired"      ?d)
          (shan/speedbar-toggle       "Speedbar"   ?n)
          (ghostel-project          "Terminal"   ?t)
          (project-eshell           "Eshell"     ?e)))
  ;; extra keys on the C-x p prefix
  (define-key project-prefix-map (kbd "t") #'ghostel-project)
  (define-key project-prefix-map (kbd "n") #'shan/speedbar-toggle)
  (define-key project-prefix-map (kbd "m") #'shan/project-mistty))

;; --- project-x: per-project window layouts --------------------------------
;; Saves the window configuration per project and restores it on switch, so
;; H-p lands back in the arrangement you left.  Layouts can be named
;; (project-x-window-state-save-as) for several per project.
;; Caveat: current frame only; a layout holding remote (TRAMP) files will
;; try to reconnect on restore -- C-g if the far end is asleep.
(use-package project-x
  :ensure t
  :after project
  :custom
  (project-x-window-list-file (no-littering-expand-var-file-name "project-window-list"))
  (project-x-auto-save-delay 5)     ; save after 5s idle (default: never)
  (project-x-save-extra-buffers t)  ; also restore magit/eshell/compilation
  :config
  ;; project-x-local-identifier defaults to ".project", matching
  ;; project-vc-extra-root-markers above -- one marker file, both systems
  (project-x-mode 1)
  ;; tab-per-project workspaces: uncomment to try (replaces tabspaces etc.)
  ;; (project-x-tabs-mode 1)
  (define-key project-prefix-map (kbd "w") #'project-x-window-state-save-as))

;; --- speedbar: built-in file tree in a side window (Emacs 31) -------------
;; replaces neotree; speedbar follows the current buffer's directory (press
;; U in the tree to go up, e.g. to the project root)
(use-package speedbar
  :ensure nil
  :custom
  (speedbar-prefer-window t)           ; side window, not a separate frame
  (speedbar-window-default-width 30)
  (speedbar-use-images nil)            ; text arrows; also works in tty frames
  (speedbar-show-unknown-files t)      ; show all files, not just recognized types
  (speedbar-indentation-width 2)
  ;; show dot-directories (.github, .config...): the default regexp hides
  ;; every name starting with a dot; this hides only . and ..
  ;; (dot-FILES are already shown -- speedbar-file-unshown-regexp only
  ;; excludes . and .. plus build/backup extensions)
  (speedbar-directory-unshown-regexp "\\`\\.\\.?\\'")
  :config
  ;; tree text at 85% of the default font, tracking font-size changes
  (add-hook 'speedbar-mode-hook
            (lambda () (face-remap-add-relative 'default :height 0.85)))

  ;; --- VC state in the tree ----------------------------------------------
  ;; Speedbar's VC support is RCS-era and needs two fixes for git:
  ;;
  ;; 1. It only looks for VC when the *current* directory contains a VCS
  ;;    dir, so anywhere below the repo root (lisp/, src/...) it silently
  ;;    does nothing.  The hook below walks up instead.
  ;; 2. Its marker means "writable and tracked", which under RCS meant
  ;;    "checked out for editing".  Under git every tracked file is
  ;;    writable, so every file would be flagged.  Redefine the predicate
  ;;    to mean "modified" so the marker actually carries information.
  (setq speedbar-vc-do-check t)
  (setq speedbar-vc-indicator " *")

  (add-hook 'speedbar-vc-directory-enable-hook
            (lambda (dir) (locate-dominating-file dir ".git")))

  (defun shan/speedbar-file-modified-p (dir name)
    "Non-nil when DIR/NAME differs from its VC state.
Replaces `speedbar-this-file-in-vc' so the speedbar marker flags
modified files rather than every tracked one."
    (ignore-errors
      (memq (vc-state (expand-file-name name dir))
            '(edited added removed conflict missing))))
  (advice-add 'speedbar-this-file-in-vc
              :override #'shan/speedbar-file-modified-p))

(defun shan/speedbar-toggle ()
  "Toggle the speedbar side window."
  (interactive)
  (require 'speedbar)
  (speedbar-window-mode))

(bind-key "C-<f8>" #'shan/speedbar-toggle)

(provide 'init-project)
