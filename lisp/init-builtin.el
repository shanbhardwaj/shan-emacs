;;; init-builtin.el --- Built-in package configuration -*- lexical-binding: t; -*-
  (use-package dired
    :ensure nil                                                ;; This is built-in, no need to fetch it.
    :custom
    (dired-listing-switches "-lah --group-directories-first")  ;; Display files in a human-readable format and group directories first.
    (dired-dwim-target t)                                      ;; Enable "do what I mean" for target directories.
    (dired-guess-shell-alist-user
     '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open") ;; Open image files with `feh' or the default viewer.
       ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open") ;; Open audio and video files with `mpv'.
       (".*" "open" "xdg-open")))                              ;; Default opening command for other files.
    (dired-kill-when-opening-new-dired-buffer t)               ;; Close the previous buffer when opening a new `dired' instance.
    :config
    (when (eq system-type 'darwin)
      (let ((gls (executable-find "gls")))                     ;; Use GNU ls on macOS if available.
        (when gls
          (setq insert-directory-program gls)))))

  ;; (use-package dired-posframe
  ;;   :ensure t
  ;;   :bind ((dired-mode-map
  ;;           ("C-*" . dired-posframe-show))))

  ;; (use-package dired-hacks
  ;;   :after dired
  ;;   :ensure nil)

  ;; (use-package erc
  ;;   :ensure nil
  ;;   :defer t ;; Load ERC when needed rather than at startup. (Load it with `M-x erc RET')
  ;;   :custom
  ;;   (erc-join-buffer 'window)                                        ;; Open a new window for joining channels.
  ;;   (erc-hide-list '("JOIN" "PART" "QUIT"))                          ;; Hide messages for joins, parts, and quits to reduce clutter.
  ;;   (erc-timestamp-format "[%H:%M]")                                 ;; Format for timestamps in messages.
  ;;   (erc-autojoin-channels-alist '((".*\\.libera\\.chat" "#emacs"))));; Automatically join the #emacs channel on Libera.Chat.

  (use-package isearch
    :ensure nil                                  ;; This is built-in, no need to fetch it.
    :config
    (setq isearch-lazy-count t)                  ;; Enable lazy counting to show current match information.
    (setq lazy-count-prefix-format "(%s/%s) ")   ;; Format for displaying current match count.
    (setq lazy-count-suffix-format nil)          ;; Disable suffix formatting for match count.
    (setq search-whitespace-regexp ".*?")        ;; Allow searching across whitespace.
    :bind (("C-s" . isearch-forward)             ;; Bind C-s to forward isearch.
           ("C-r" . isearch-backward)))          ;; Bind C-r to backward isearch.

  (use-package eldoc
    :ensure nil          ;; This is built-in, no need to fetch it.
    :init
    (global-eldoc-mode))

  ;; TODO: add eldoc-box

  (use-package flymake
    :ensure nil          ;; This is built-in, no need to fetch it.
    :defer t
    :hook (prog-mode . flymake-mode)
    :custom
    (flymake-margin-indicators-string
     '((error "!»" compilation-error) (warning "»" compilation-warning)
       (note "»" compilation-info))))

  (use-package which-key
    :ensure nil     ;; This is built-in, no need to fetch it.
    :defer t        ;; Defer loading Which-Key until after init.
    :hook
    (after-init . which-key-mode)) ;; Enable which-key mode after initialization.

  (use-package project
    :ensure nil                                  ;; This is built-in, no need to fetch it.
    :custom
    ;; Commands offered in the `C-x p p' (project-switch-project) menu.
    (project-switch-commands
     '((project-find-file "Find file" ?f)          ;; Find a file in the project.
       (project-find-regexp "Find regexp" ?g)      ;; Grep across the project.
       (project-dired "Dired" ?d)                  ;; Open Dired at the project root.
       (project-switch-to-buffer "Buffer" ?b)      ;; Switch among the project's buffers.
       (project-eshell "Eshell" ?e)                ;; Eshell at the project root.
       (magit-project-status "Magit" ?m))))        ;; Open Magit for the project.

