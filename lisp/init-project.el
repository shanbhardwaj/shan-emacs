;;; init-project.el --- project.el configuration -*- lexical-binding: t; -*-

;; Built-in project.el is the single source of project awareness:
;; claude-code-ide resolves its session root through it, ghostel provides
;; project-scoped terminals, consult-project-extra reads its project list,
;; and neotree is taught to follow it below.

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
          (shan/neotree-project-toggle "Neotree" ?n)
          (ghostel-project          "Terminal"   ?t)
          (project-eshell           "Eshell"     ?e)))
  ;; extra keys on the C-x p prefix
  (define-key project-prefix-map (kbd "t") #'ghostel-project)
  (define-key project-prefix-map (kbd "n") #'shan/neotree-project-toggle))

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

;; --- neotree: root the tree at the current project ------------------------
(defun shan/neotree-project-toggle ()
  "Toggle neotree rooted at the current project.
Falls back to `default-directory' outside a project; when opening,
reveal the current file in the tree."
  (interactive)
  (if (and (featurep 'neotree) (neo-global--window-exists-p))
      (neotree-hide)
    (let ((root (if-let* ((proj (project-current)))
                    (project-root proj)
                  default-directory))
          (file (buffer-file-name)))
      (neotree-dir root)
      (when file (neotree-find file)))))

;; replace plain neotree-toggle: same key, but project-rooted
(bind-key "C-<f8>" #'shan/neotree-project-toggle)

(provide 'init-project)
