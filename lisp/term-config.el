;;; term-config.el --- Terminal configuration -*- lexical-binding: t; -*-

;; mistty opens as a drawer on the bottom 30% of the frame instead of a
;; full-height split; placement is handled by `display-buffer-alist', so
;; no shell-pop-style package is needed (shell-pop was flaky here before)
(add-to-list 'display-buffer-alist
             '((derived-mode . mistty-mode)
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               (window-height . 0.3)))

(defun shan/mistty-toggle ()
  "Pop a mistty shell in the bottom drawer; hide it if visible.
`mistty' reuses a live session, so toggling preserves the shell."
  (interactive)
  (if-let* ((win (seq-find (lambda (w)
                             (with-current-buffer (window-buffer w)
                               (derived-mode-p 'mistty-mode)))
                           (window-list))))
      (delete-window win)
    (mistty)))

(use-package mistty
  :ensure t
  :commands (mistty)
  :bind (("C-c s" . shan/mistty-toggle)
         ("H-`"   . shan/mistty-toggle)
         :map mistty-prompt-map
         ;; keys the shell handles instead of Emacs (history navigation)
         ("M-<up>"    . mistty-send-key)
         ("M-<down>"  . mistty-send-key)
         ("M-<left>"  . mistty-send-key)
         ("M-<right>" . mistty-send-key)))

(provide 'term-config)
