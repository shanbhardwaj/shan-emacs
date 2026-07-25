;;; init-ui.el --- UI configuration -*- lexical-binding: t; -*-
;;; Commentary:

;; (set-face-attribute 'default nil :font "Monaco Nerd Font Mono" :height 120 :weight 'regular)
;; (set-face-attribute 'default nil :font "Iosevka Nerd Font Mono" :height 160 :weight 'regular)
;; (set-face-attribute 'default nil :font "CaskaydiaMono Nerd Font" :height 140 :weight 'regular)
;; (set-face-attribute 'default nil :font "CommitMono Nerd Font" :height 140 :weight 'regular)
;; (set-face-attribute 'default nil :font "Monaco" :height 140 :weight 'regular)
;; (set-face-attribute 'default nil :font "InputMonoNarrow Light" :height 140 :weight 'regular)

;; (use-package spacious-padding
;;   :ensure t
;;   :config
;;   (setq spacious-padding-widths
;;         '( :internal-border-width 12
;;            :header-line-width 4
;;            :mode-line-width 4
;;            :tab-width 4
;;            :right-divider-width 6
;;            :scroll-bar-width 2))
;;   (spacious-padding-mode 0))

(use-package show-font
  :ensure t
  :commands (show-font-list show-font-select-preview show-font-tabulated))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
    		doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-horizon t)
  ;; doom-horizon's inactive mode-line text is bg lightened 20% -- too
  ;; faint to read at the reduced 0.8 mode-line height; use dimmed fg
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (doom-darken (doom-color 'fg) 0.35))
  ;; header-line inherits mode-line in doom themes; the 0.8 mode-line
  ;; height would shrink it too and misalign mu4e's column headers, so
  ;; pin it to the default font size (absolute height overrides inherit)
  (set-face-attribute 'header-line nil
                      :height (face-attribute 'default :height))
  ;; break the doom<->gnus face inheritance cycle: gnus's default spec for
  ;; gnus-group-news-low inherits -low-empty while doom-themes points
  ;; -low-empty back at -low; once gnus faces load (via mu4e), every new
  ;; frame creation errors -- killing posframes and vertico with them.
  ;; Rewriting the *default* specs removes the back-edge at the root.
  (defun shan/break-gnus-face-cycle ()
    (face-spec-set 'gnus-group-news-low
                   '((t :inherit gnus-group-mail-1)) 'face-defface-spec)
    (face-spec-set 'gnus-group-news-low-empty
                   '((t :inherit gnus-group-mail-1 :weight normal))
                   'face-defface-spec))
  (with-eval-after-load 'gnus (shan/break-gnus-face-cycle))
  (doom-themes-visual-bell-config)     ;; Enable flashing mode-line on errors
  (require 'org-indent)
  ;;(doom-themes-org-configure)
	)

;; Install and configure modus-themes
(use-package ef-themes
  :ensure nil)

;; (use-package auto-dark
;;   :ensure t
;;   :custom
;;   (auto-dark-themes '((doom-one) (doom-one-light)))
;;   (auto-dark-polling-interval-seconds 300)
;;   (auto-dark-allow-osascript t)
;;   (auto-dark-allow-powershell nil)
;;   ;; (auto-dark-detection-method nil) ;; dangerous to be set manually
;;   :hook
;;   (auto-dark-dark-mode
;;    . (lambda ()
;;        ;; something to execute when dark mode is detected
;;        ))
;;   (auto-dark-light-mode
;;    . (lambda ()
;;        ;; something to execute when light mode is detected
;;        ))
;;   :init (auto-dark-mode))

(defun disable-active-themes ()
  "Disable any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(bind-key "s-<f12>" 'consult-theme)
(bind-key "s-<f11>" 'disable-active-themes)

(defun hide-mode-line-in-term-shell-comint ()
  "Hide the mode line in term, shell, and comint buffers."
  (when (derived-mode-p 'term-mode 'shell-mode 'comint-mode 'eshell-mode)
    (setq mode-line-format nil)))

(dolist (hook '(term-mode-hook shell-mode-hook comint-mode-hook eshell-mode-hook))
  (add-hook hook 'hide-mode-line-in-term-shell-comint))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  ;; looks
  (doom-modeline-height 20)
  (doom-modeline-bar-width 2)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-time-analogue-clock nil)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-encoding 'nondefault) ; only shown when not utf-8/LF
  (doom-modeline-vcs-max-length 15)
  ;; performance
  (doom-modeline-enable-word-count nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-check 'simple)
  (doom-modeline-env-version nil) ; skips spawning rbenv/nvm etc. on file open
  (doom-modeline-github nil)      ; no github polling timer
  (doom-modeline-battery nil)
  ;; integrations
  (doom-modeline-mu4e nil))       ; unread count comes from mu4e-modeline-mode instead

;; (set-face-attribute 'mode-line nil :height 0.8)
;; (set-face-attribute 'mode-line-inactive nil :height 0.8)

;; (defun reset-modeline()
;;   "Reset the modeline to 12pt font"
;;   (interactive)
;; 	(set-face-attribute 'mode-line nil :height 0.8)
;; 	(set-face-attribute 'mode-line-inactive nil :height 0.8))

;; (defun switch-theme (theme)
;;   "Disables any currently active themes and loads THEME."
;;   ;; This interactive call is taken from `load-theme'
;;   (interactive
;;    (list
;;     (intern (completing-read "Load custom theme: "
;;                              (mapc 'symbol-name
;;                                    (custom-available-themes))))))
;;   (let ((enabled-themes custom-enabled-themes))
;;     (mapc #'disable-theme custom-enabled-themes)
;;     (load-theme theme t)
;;     (reset-modeline)
;;     ))


;; (with-current-buffer " *Echo Area 0*" (face-remap-add-relative 'default '(:height 0.9)))

;; Use monospaced font faces in current buffer
;; (defun my-term-mode-face ()
;;   "Set a fixed width (monospace) font in current buffer"
;;   (interactive)
;;   (setq buffer-face-mode-face '(:height 0.8))
;;   (buffer-face-mode))

;; ;; Customize the minibuffer
;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
;; (defun my-minibuffer-setup ()
;; 	""
;; 	(set (make-local-variable 'face-remapping-alist)
;;        '((default :height 0.8))))

;; (defun fontify-frame (frame)
;;   (interactive)
;;   (if sys/macp
;;       (progn
;;         (if (> (x-display-pixel-width) 4000)
;;             (set-frame-parameter frame 'font "Monaco 14") ;; 5k Display
;;           (set-frame-parameter frame 'font "Monaco 12")))))

;; ;; Fontify current frame
;; (fontify-frame nil)

;; ;; Fontify any future frames
;; (push 'fontify-frame after-make-frame-functions)
