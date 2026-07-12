;;; term-config.el --- Terminal configuration -*- lexical-binding: t; -*-
(require 'use-package) ;; Ensure `use-package` is loaded
(use-package mistty
  :bind (("C-c s" . mistty)

         ;; bind here the shortcuts you'd like the
         ;; shell to handle instead of Emacs.
         :map mistty-prompt-map

         ;; fish: directory history
         ("M-<up>" . mistty-send-key)
         ("M-<down>" . mistty-send-key)
         ("M-<left>" . mistty-send-key)
         ("M-<right>" . mistty-send-key)))

(provide 'term-config)
