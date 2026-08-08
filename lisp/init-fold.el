;;; init-fold.el --- Code folding: treesit-fold engine + kirigami frontend -*- lexical-binding: t; -*-
;;; Commentary:
;; Structural code folding for source buffers.
;;
;; Layering:
;;   - treesit-fold : the engine — folds off the tree-sitter syntax tree
;;                    (accurate, language-aware) in treesit-backed major modes.
;;   - hideshow     : fallback engine for prog modes not backed by tree-sitter.
;;   - kirigami     : one keymap (C-c z ...) that dispatches to whichever engine
;;                    is active in the current buffer.  Backends must be enabled
;;                    separately (below); kirigami only routes the commands.
;;
;; NOTE: magit diff/status buffers do NOT use these — they fold via
;; magit-section.  There, use TAB (a file/hunk) and M-1/M-2/M-3/M-4 (detail
;; level for the whole buffer).
;;; Code:

;; Engine: tree-sitter structural folding (built-in treesit.el, Emacs 29.1+).
;; global-treesit-fold-mode only activates in buffers that have a tree-sitter
;; parser, so it is a no-op elsewhere.
(use-package treesit-fold
  :ensure t
  :hook (after-init . global-treesit-fold-mode)
  :config
  (when (fboundp 'global-treesit-fold-indicators-mode)
    (global-treesit-fold-indicators-mode 1))) ; fringe markers for foldable/folded

;; Fallback engine for non-tree-sitter prog modes (brace/block languages).
(use-package hideshow
  :ensure nil                                  ; built-in
  :hook (prog-mode . hs-minor-mode))

;; Unified frontend: define the fold keys once; kirigami detects the active
;; backend and dispatches.  Works in source buffers, org, markdown, etc.
(use-package kirigami
  :ensure t
  :hook (after-init . kirigami-global-mode)
  :bind (("C-c z o" . kirigami-open-fold)      ; open fold at point
         ("C-c z O" . kirigami-open-fold-rec)  ; open recursively
         ("C-c z c" . kirigami-close-fold)     ; close fold at point
         ("C-c z a" . kirigami-toggle-fold)    ; toggle fold at point
         ("C-c z r" . kirigami-open-folds)     ; open all folds in buffer
         ("C-c z m" . kirigami-close-folds)))  ; close all folds in buffer

(provide 'init-fold)
;;; init-fold.el ends here
