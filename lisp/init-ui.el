;;; init-ui.el --- UI configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Sections: Theme | Fonts | Mode line | Misc UI
;; The default font is set in init.el (Typography); font *browsing* tools
;; and old font experiments live in the Fonts section below.
;; Theme-switching keys (H-t, s-<f12>, s-<f11>) are in init-settings.el.

;;; ============================================================ Theme

;; The active theme.  Change this one line (then restart or re-eval this
;; file) to switch permanently; `consult-theme' (H-t) previews live.
(defvar shan/theme 'doom-horizon
  "Theme loaded at startup by `init-ui.el'.")

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t      ; if nil, bold is universally disabled
        doom-themes-enable-italic t)   ; if nil, italics is universally disabled
  (load-theme shan/theme t)
  (doom-themes-visual-bell-config)     ; flash the mode-line on errors
  (require 'org-indent)
  ;; (doom-themes-org-configure)

  ;; --- theme fixups ---------------------------------------------------
  ;; doom-horizon's inactive mode-line text is bg lightened 20% -- too
  ;; faint to read at a reduced mode-line height; use a dimmed fg instead
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (doom-darken (doom-color 'fg) 0.35))
  ;; header-line inherits mode-line in doom themes; a shrunken mode-line
  ;; would shrink it too and misalign mu4e's column headers, so pin it to
  ;; the default font size (an absolute height overrides the inherit)
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
  (with-eval-after-load 'gnus (shan/break-gnus-face-cycle)))

;; alternative theme families, available to consult-theme (H-t)
(use-package ef-themes
  :ensure nil)

(defun disable-active-themes ()
  "Disable any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;; follow the macOS light/dark setting (disabled; set auto-dark-themes
;; and re-enable if you want automatic switching)
;; (use-package auto-dark
;;   :ensure t
;;   :custom
;;   (auto-dark-themes '((doom-one) (doom-one-light)))
;;   (auto-dark-polling-interval-seconds 300)
;;   (auto-dark-allow-osascript t)
;;   (auto-dark-allow-powershell nil)
;;   :init (auto-dark-mode))

;;; ============================================================ Fonts

;; Default font: init.el, Typography section.
;; Browse installed fonts: M-x show-font-list / show-font-select-preview
;; Try one live (this frame only): M-x set-frame-font
(use-package show-font
  :ensure t
  :commands (show-font-list show-font-select-preview show-font-tabulated))

;; past font choices, kept for quick A/B:
;; (set-face-attribute 'default nil :font "Monaco Nerd Font Mono"   :height 120)
;; (set-face-attribute 'default nil :font "Iosevka Nerd Font Mono"  :height 160)
;; (set-face-attribute 'default nil :font "CaskaydiaMono Nerd Font" :height 140)
;; (set-face-attribute 'default nil :font "CommitMono Nerd Font"    :height 140)
;; (set-face-attribute 'default nil :font "InputMonoNarrow Light"   :height 140)

;;; ============================================================ Mode line

;; Smaller mode-line text without a shorter mode-line: :height 0.85 shrinks
;; the font, and an invisible :box (color = the face's own background) pads
;; the bar back to size. Runs on every theme change so the padding color
;; always tracks the current theme's background.
(defun shan/modeline-compact-text (&rest _)
  "Give the mode-line 0.85-height text with height-preserving padding."
  (dolist (face '(mode-line mode-line-inactive))
    (set-face-attribute face nil
                        :height 0.85
                        :box `(:line-width (1 . 4)
                               :color ,(or (face-background face nil t)
                                           (face-background 'default nil t))))))
(add-hook 'enable-theme-functions #'shan/modeline-compact-text)
(shan/modeline-compact-text)

;; Clean, package-free mode-line.  Every construct here is either a C-level
;; %-escape or a variable kept fresh by hooks (vc-mode, misc-info) --
;; nothing runs per redisplay, so there is none of the modeline-package
;; overhead.  Dropped from the default: coding-system indicator, input
;; method, minor-mode list (which-key/diminish cover discovery), %p.
(setq project-mode-line t)              ; built-in cached project segment
(setq vc-display-status 'no-backend)    ; "-master" not "Git-master"; the
                                        ; state char (-/:/@) still shows
(setq-default mode-line-format
              '("%e" mode-line-front-space
                (:propertize "%*%@" face shadow) ; modified + remote flags
                " "
                (:propertize "%b" face mode-line-buffer-id)
                "  %l:%c"
                mode-line-format-right-align
                (project-mode-line project-mode-line-format)
                "  " mode-name
                (vc-mode vc-mode)
                "  " mode-line-misc-info
                mode-line-end-spaces))

;; hide the mode line entirely in terminal-ish buffers
(defun hide-mode-line-in-term-shell-comint ()
  "Hide the mode line in term, shell, and comint buffers."
  (when (derived-mode-p 'term-mode 'shell-mode 'comint-mode 'eshell-mode)
    (setq mode-line-format nil)))

(dolist (hook '(term-mode-hook shell-mode-hook comint-mode-hook eshell-mode-hook))
  (add-hook hook 'hide-mode-line-in-term-shell-comint))

;; replaced by the hand-rolled mode-line above; kept for reference
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode)
;;   :custom
;;   (doom-modeline-height 20)
;;   (doom-modeline-bar-width 2)
;;   (doom-modeline-icon t)
;;   (doom-modeline-major-mode-icon t)
;;   (doom-modeline-major-mode-color-icon t)
;;   (doom-modeline-buffer-file-name-style 'truncate-upto-project)
;;   (doom-modeline-minor-modes nil)
;;   (doom-modeline-buffer-encoding 'nondefault)
;;   (doom-modeline-vcs-max-length 15)
;;   (doom-modeline-enable-word-count nil)
;;   (doom-modeline-indent-info nil)
;;   (doom-modeline-check 'simple)
;;   (doom-modeline-env-version nil)
;;   (doom-modeline-github nil)
;;   (doom-modeline-battery nil)
;;   (doom-modeline-mu4e nil))

;;; ============================================================ Misc UI

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
