;; mu4e thread fast folding -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
(require 'mu4e)

(defun mu4e-fast-folding-info (msg)
  (let* ((thread (mu4e-message-field msg :thread))
         (prefix (mu4e~headers-thread-prefix thread))
         (unread (memq 'unread (mu4e-message-field msg :flags))))
    (concat
     (if (= (length prefix) 0) " " " ") ;; Normal space vs Non-breaking space
     (if unread "•" " ")))) ;; Specific character to later detect unread

(add-to-list 'mu4e-header-info-custom
             '(:fast-folding . (:name "fast-folding"
                         :shortname ""
                         :function mu4e-fast-folding-info)))

(setq mu4e-headers-fields '((:fast-folding . 2)
                            (:human-date . 12)
                            (:flags . 6)
                            (:mailing-list . 10)
                            (:from . 22)
                            (:subject)))

(defun mu4e-fast-folding-is-unfolded-child ()
  "Check if the line at point is an unfolded thread child.
This is detected by the presence of non-breaking space."

  (interactive)
  (save-excursion
    (beginning-of-line)
    (and (not (mu4e-fast-folding-is-folded-children))
         (search-forward " " (line-end-position) t))))

(defun mu4e-fast-folding-is-folded-children ()
  "Check if the line at point is a folded thread.
This is detected by the presence of an overlay with value 'overlay."

  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((overlays (overlays-at (point)))
          (found nil))
      (while overlays
        (if (overlay-get (car overlays) 'overlay)
            (setq found t))
        (setq overlays (cdr overlays)))
      found)))

(defun mu4e-fast-folding-is-root ()
  "Check if the line at point is a thread root."

  (interactive)
  (and (not (mu4e-fast-folding-is-unfolded-child))
       (not (mu4e-fast-folding-is-folded-children))))

(defun mu4e-fast-folding-is-unread ()
  "Check if the line at point is an unread message."

    (save-excursion
      (beginning-of-line)
      (search-forward "•" (line-end-position) t)))

(defun mu4e-fast-folding-thread-toggle ()
  "Toggle thread at point."

  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (mu4e-fast-folding-is-root)
        (forward-line))
    (cond ((mu4e-fast-folding-is-folded-children)
           (mu4e-fast-folding-thread-unfold))
          ((mu4e-fast-folding-is-unfolded-child)
           (mu4e-fast-folding-thread-fold)))))


(defun mu4e-fast-folding-thread-unfold ()
  "Unfold thread at point."

  (interactive)
  (if (mu4e-fast-folding-is-root)
      (forward-line))

  (let ((overlays (overlays-at (point))))
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay 'overlay)
            (delete-overlay (overlay-get overlay 'overlay))))
      (setq overlays (cdr overlays)))))


(defun mu4e-fast-folding-thread-fold ()
  "Fold thread at point."

  (interactive)

  ;; Move to thread start
  (beginning-of-line)
  (while (and (> (point) (point-min))
              (mu4e-fast-folding-is-unfolded-child))
    (forward-line -1))
  (forward-line +1)

  ;; Hide all children, count them  and count unread
  (beginning-of-line)
  (let ((start (point))
        (end (+ (point) 1))
        (unread 0)
        (count 0))
    (while (and (< (point) (point-max))
                (mu4e-fast-folding-is-unfolded-child))

        ;; Count unread
        (beginning-of-line)
        (if (mu4e-fast-folding-is-unread)
            (setq unread (+ unread 1)))

        ;; Count thread
        (setq count (+ count 1))

        ;; Set new end for the overlay
        (setq end (+ (line-end-position) 1))
        (forward-line +1)
        (beginning-of-line))

      ;; Add overlay
      (let* ((overlay (make-overlay start (- end 1)))
             (face (if (> unread 0) 'mu4e-unread-face 'mu4e-system-face))
             (text (if (> unread 0)
                       (format "      --- %d hidden messages (%d unread) ---      " count unread)
                     (format "      --- %d hidden messages ---      " count))))

        ;; No overlay if only 1 child
        (when (> count 1)
          (overlay-put overlay 'display (propertize text 'face face))
          (overlay-put overlay 'overlay overlay)))))


(defun mu4e-fast-folding-thread-fold-all ()
  "Fold all threads independently of their current state."

  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (mu4e-fast-folding-thread-fold)
      (forward-line))))

(defun mu4e-fast-folding-thread-unfold-all ()
  "Unfold all threads, independently of their current state."

  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (mu4e-fast-folding-thread-unfold)
      (forward-line))))

(defvar mu4e-fast-folding-thread-folding-state nil
  "Global folding state")

(defun mu4e-fast-folding-thread-toggle-all ()
  "Toggle global folding state."

  (interactive)
  (when mu4e-headers-include-related
    (setq mu4e-fast-folding-thread-folding-state
          (not mu4e-fast-folding-thread-folding-state))
    (mu4e-fast-folding-thread-apply-folding)))


(defun mu4e-fast-folding-thread-apply-folding ()
  "Apply folding according to the global folding state."

  (interactive)
  (if mu4e-fast-folding-thread-folding-state
      (mu4e-fast-folding-thread-fold-all)
    (mu4e-fast-folding-thread-unfold-all)))

(add-hook 'mu4e-headers-found-hook
          #'mu4e-fast-folding-thread-apply-folding)

(define-key mu4e-headers-mode-map (kbd "TAB")
  #'mu4e-fast-folding-thread-toggle)

(define-key mu4e-headers-mode-map (kbd "<backtab>")
  #'mu4e-fast-folding-thread-toggle-all)
