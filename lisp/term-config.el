;;; term-config.el --- Terminal configuration -*- lexical-binding: t; -*-

;; mistty opens as a drawer on the bottom 30% of the frame instead of a
;; full-height split; placement is handled by `display-buffer-alist', so
;; no shell-pop-style package is needed (shell-pop was flaky here before)
(add-to-list 'display-buffer-alist
             '((derived-mode . mistty-mode)
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               (window-height . 0.3)))

(defun shan/mistty--buffer-name (root)
  "Buffer name for the mistty shell belonging to project ROOT."
  (format "*mistty[%s]*"
          (file-name-nondirectory (directory-file-name root))))

(defun shan/project-mistty ()
  "Open (or switch to) the mistty shell for the current project.
One shell per project, rooted at its top directory and named after it,
so switching projects gets you that project's shell rather than whichever
one happened to be open."
  (interactive)
  (if-let* ((proj (project-current)))
      (let* ((root (project-root proj))
             (name (shan/mistty--buffer-name root))
             (buf (get-buffer name)))
        (if (buffer-live-p buf)
            (pop-to-buffer buf)
          ;; mistty-create names the buffer from `mistty-buffer-name', so
          ;; bind that (and the directory) for this one call
          (let ((default-directory root)
                (mistty-buffer-name
                 (list (lambda () (format "mistty[%s]"
                                          (file-name-nondirectory
                                           (directory-file-name root)))))))
            (mistty-create))))
    (user-error "Not in a project; use %s for a plain shell"
                (key-description (where-is-internal #'shan/mistty-toggle nil t)))))

(defun shan/mistty-toggle ()
  "Toggle the bottom mistty drawer for the current project.
Hides it when visible; otherwise opens this project's shell (or a plain
one outside a project).  The shell keeps running while hidden."
  (interactive)
  (if-let* ((win (seq-find (lambda (w)
                             (with-current-buffer (window-buffer w)
                               (derived-mode-p 'mistty-mode)))
                           (window-list))))
      (delete-window win)
    (if (project-current) (shan/project-mistty) (mistty))))

(use-package mistty
  :ensure t
  :commands (mistty mistty-create)
  :bind (("C-c s" . shan/mistty-toggle)
         ("H-`"   . shan/mistty-toggle)
         ("C-c S" . shan/project-mistty)   ; full window, not the drawer
         :map mistty-prompt-map
         ;; keys the shell handles instead of Emacs (history navigation)
         ("M-<up>"    . mistty-send-key)
         ("M-<down>"  . mistty-send-key)
         ("M-<left>"  . mistty-send-key)
         ("M-<right>" . mistty-send-key)))

(provide 'term-config)
