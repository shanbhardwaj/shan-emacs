;;; init-servers.el --- Dev-server launchers: prodigy (kulcare) + kitty (any project) -*- lexical-binding: t; -*-
;;; Commentary:
;; Two ways to start local app servers:
;;
;;   C-c v p  — prodigy dashboard (Emacs-managed).  kulcare only, for when you
;;              want the logs inside Emacs.  `s'/`S'/`r' start/stop/restart,
;;              `$' shows a running service's log buffer.
;;
;;   C-c v s  — pick a project and open its dev stack in a NEW TAB of the
;;              already-running kitty, as a split layout (backend/worker in a
;;              left column, frontends in a right column).  Projects are
;;              discovered from `<name>-dev.sh' scripts in
;;              `shan/kitty-sessions-dir' — add a project by dropping in a
;;              script (copy an existing one), no elisp change needed.  Each
;;              script drives the running kitty via remote control (`kitty @'),
;;              so it reuses your current window instead of spawning a new one.
;;
;; Port map (all can run at once — see the session files): backends
;; kulcare 4000 / commander 3011 / rituality 3200; frontends kulcare 3000 /
;; commander 3100 / rituality 5173.  Postgres (:5432) and Redis (:6379) are
;; shared single instances (one DB per project).
;;; Code:

(defvar shan/kulcare-root
  (expand-file-name "~/projects/work/kulcare/kulcare-monorepo")
  "Path to the Kulcare monorepo checkout.")

;; --- Option A: prodigy (Emacs-managed) — kulcare --------------------------
(use-package prodigy
  :ensure t
  :bind (("C-c v p" . prodigy))
  :config
  (prodigy-define-service
    :name "kulcare backend (:4000)"
    :command "mise"
    :args '("exec" "--" "bin/dev")
    :cwd (expand-file-name "backend" shan/kulcare-root)
    :env '(("PORT" "4000"))
    :ready-message "Listening on"
    :stop-signal 'sigint
    :tags '(kulcare backend))

  (prodigy-define-service
    :name "kulcare sidekiq"
    :command "mise"
    :args '("exec" "--" "bundle" "exec" "sidekiq")
    :cwd (expand-file-name "backend" shan/kulcare-root)
    :tags '(kulcare backend))

  (prodigy-define-service
    :name "kulcare frontend (shell :3000)"
    :command "pnpm"
    :args '("run" "dev:shell")
    :cwd (expand-file-name "frontend" shan/kulcare-root)
    :ready-message "ready in"
    :tags '(kulcare frontend))

  (prodigy-define-service
    :name "kulcare patient (:5173)"
    :command "pnpm"
    :args '("dev")
    :cwd (expand-file-name "patient" shan/kulcare-root)
    :ready-message "ready in"
    :tags '(kulcare patient)))

;; --- Option B: kitty per-project sessions (external, any project) ---------
(defvar shan/kitty-sessions-dir
  (expand-file-name "~/.config/kitty/sessions/")
  "Directory holding per-project `<name>-dev.sh' kitty launcher scripts.")

(defun shan/project-dev--projects ()
  "Alist of (NAME . SCRIPT) discovered in `shan/kitty-sessions-dir'.
Only matches `<name>-dev.sh'; the shared `_lib.sh' is skipped."
  (mapcar (lambda (f)
            (cons (replace-regexp-in-string "-dev\\.sh\\'" ""
                                            (file-name-nondirectory f))
                  f))
          (file-expand-wildcards
           (expand-file-name "*-dev.sh" shan/kitty-sessions-dir))))

(defun shan/project-dev (name)
  "Open project NAME's dev-server stack in a new tab of the running kitty.
Projects come from `<name>-dev.sh' scripts in `shan/kitty-sessions-dir'; each
opens a split-layout tab (backend/worker left, frontends right) in the current
kitty via remote control.  Add a project by dropping in a script.  To stop a
project's servers, close its tab (or Ctrl-C each pane)."
  (interactive
   (let ((projects (shan/project-dev--projects)))
     (unless projects
       (user-error "No *-dev.sh scripts in %s" shan/kitty-sessions-dir))
     (list (completing-read "Project dev servers: " projects nil t))))
  (unless (executable-find "kitty")
    (user-error "kitty is not on exec-path"))
  (let ((script (cdr (assoc name (shan/project-dev--projects)))))
    (unless (and script (file-exists-p script))
      (user-error "No launcher script for %S" name))
    (start-process (format "dev-%s" name) "*project-dev*" "bash" script)
    (message "Opening %s dev servers in kitty (see *project-dev* on error)." name)))

(global-set-key (kbd "C-c v s") #'shan/project-dev)

(provide 'init-servers)
;;; init-servers.el ends here
