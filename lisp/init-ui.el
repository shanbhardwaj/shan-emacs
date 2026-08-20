;;; init-ui.el --- UI configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Sections: Theme | Fonts | Mode line | Misc UI
;; The default font is set in init.el (Typography); font *browsing* tools
;; and old font experiments live in the Fonts section below.
;; Theme-switching keys (H-t, s-<f12>, s-<f11>) are in init-settings.el.

;;; ============================================================ Theme

;; The active theme.  Change this one line (then restart or re-eval this
;; file) to switch permanently; `consult-theme' (H-t) previews live.
(defvar shan/theme 'doom-ayu-dark
  "Theme loaded at startup by `init-ui.el'.
auto-dark switches this immediately to match the system appearance; it
matters only for the moment before that, so keep it as the dark half of
`auto-dark-themes'.  Previously doom-horizon.")

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
  ;; faint to read at a reduced mode-line height; use a dimmed fg instead.
  ;; These fixups are theme-clobbered, so they live in a function that
  ;; re-runs after every auto-dark theme switch (see auto-dark below).
  ;; Read theme colours from a GUI frame, never the selected one: tty
  ;; frames have their surfaces cleared to "unspecified-*" (see
  ;; shan/tty-use-terminal-colors), and these setters write globally, so
  ;; reading a tty value here would push it onto every frame.
  (defun shan/graphic-frame ()
    (seq-find #'display-graphic-p (frame-list)))

  (defun shan/apply-theme-fixups (&rest _)
    "Re-apply face fixups that loading a theme clobbers."
    (let ((gui (shan/graphic-frame)))
      ;; dimmed-but-readable inactive mode-line text, derived from the
      ;; current theme's foreground (works for dark and light themes)
      (let ((fg (face-attribute 'default :foreground gui)))
        (when (and (stringp fg) (string-prefix-p "#" fg))
          (set-face-attribute 'mode-line-inactive nil
                              :foreground (doom-darken fg 0.35))))
      ;; header-line inherits mode-line; pin it to the default font size so
      ;; a shrunken mode-line can't misalign mu4e's column headers
      (let ((h (face-attribute 'default :height gui)))
        (when (numberp h)
          (set-face-attribute 'header-line nil :height h)))))
  ;; enable-theme-functions covers auto-dark AND manual H-t theme switches
  (add-hook 'enable-theme-functions #'shan/apply-theme-fixups)
  (shan/apply-theme-fixups)
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
  ;; terminal frames: let the terminal's theme provide the canvas -- clear
  ;; the Emacs theme's default fg/bg (and related surfaces) in tty frames
  ;; so kitty/ghostty colors show through; GUI frames keep the full theme
  ;; A daemon has one theme for every frame, but the terminal it is
  ;; displayed in has its own.  When the two disagree in lightness (Emacs
  ;; on ef-summer while kitty is pinned to a dark catppuccin, say) the
  ;; theme's opaque surfaces -- mode line, header line, tab bar -- show up
  ;; as light slabs on a dark canvas.  Clearing their backgrounds in tty
  ;; frames lets the terminal's own colours through, so terminal Emacs
  ;; stays legible whatever the terminal theme is.  GUI frames keep the
  ;; full theme.
  (defvar shan/tty-transparent-faces
    '(default fringe line-number line-number-current-line
      mode-line mode-line-inactive mode-line-active
      header-line header-line-highlight
      tab-bar tab-line vertical-border window-divider)
    "Faces whose background is cleared in tty frames.
Deliberately excludes `region' and `hl-line': those need a background to
do their job, and a light-theme highlight still reads on a dark terminal.")

  (defun shan/tty-use-terminal-colors (&optional frame)
    (let ((frame (or frame (selected-frame))))
      (unless (display-graphic-p frame)
        (set-face-foreground 'default "unspecified-fg" frame)
        (dolist (face shan/tty-transparent-faces)
          (when (facep face)
            (set-face-background face "unspecified-bg" frame))))))
  (add-hook 'after-make-frame-functions #'shan/tty-use-terminal-colors)
  ;; also re-apply on theme change: a new theme repaints these faces
  (add-hook 'enable-theme-functions
            (lambda (&rest _)
              (dolist (f (frame-list))
                (unless (display-graphic-p f)
                  (shan/tty-use-terminal-colors f))))))

;; alternative theme families, available to consult-theme (H-t)
(use-package ef-themes
  :ensure t)   ; auto-dark loads ef-summer from here; :ensure nil never installs it

(defun disable-active-themes ()
  "Disable any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;; follow the macOS light/dark setting.  Note the kitty auto-themes are
;; still generated from the Horizon palettes, so while this pair is in use
;; Emacs and the terminal no longer match; regenerate those (or switch
;; back) if the mismatch grates.
;; trust locally installed themes (no load-theme confirmation prompts;
;; needed for auto-dark to switch themes unattended)
(setq custom-safe-themes t)

(use-package auto-dark
  :ensure t
  :after doom-themes
  :custom
  ;; trying the ayu pair for a while; previous: (doom-horizon) (ef-summer)
  (auto-dark-themes '((doom-ayu-dark) (doom-ayu-light)))
  (auto-dark-allow-osascript t)
  :config
  ;; face fixups re-run via enable-theme-functions (see doom-themes block)
  (auto-dark-mode 1))

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
  "Give the mode-line 0.85-height text with height-preserving padding.
Colours are read from a GUI frame: tty frames clear these backgrounds to
\"unspecified-bg\", and these setters write globally."
  (let ((gui (shan/graphic-frame)))
    (dolist (face '(mode-line mode-line-inactive))
      (let ((bg (or (face-background face gui t)
                    (face-background 'default gui t))))
        (set-face-attribute face nil :height 0.85)
        (when (and (stringp bg) (string-prefix-p "#" bg))
          (set-face-attribute face nil
                              :box `(:line-width (1 . 4) :color ,bg)))))))
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
