;;; init-ui.el --- UI configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Sections: Typography | Theme | Fonts | Mode line | Misc UI
;; The default font, the size commands and the fixed chrome height are in
;; Typography; the font *picker* is in Fonts.
;; Theme-switching keys (H-t, s-<f12>, s-<f11>) are in init-settings.el.

;;; ============================================================ Typography

;; Moved here from init.el so all UI lives in one file.
;;
;; Two independent sizes:
;;   - the TEXT size, `shan/font-override' or `shan/font-preferences'
;;   - the CHROME size, `shan/ui-font-height', fixed
;; Changing the text size must not move the mode line or header line, so the
;; chrome height is absolute rather than the 0.85 *relative* height it used
;; to be.  A relative height silently tracks the default face, which is why
;; every global size change used to drag the chrome along with it.

(defcustom shan/ui-font-height 120
  "Height of the mode line and header line, in 1/10 pt.  120 = 12pt.
Absolute on purpose: text size changes must leave the chrome alone."
  :type 'integer
  :group 'shan)

;; First family actually installed wins, so one config serves both machines:
;; SF Mono is macOS-only (Apple licence) and Caskaydia is what the Linux box
;; has.  Height travels with the family because the two displays want
;; different sizes -- hardcoding either made every `git pull' conflict.
(defvar shan/font-preferences
  '(("CaskaydiaMono Nerd Font" . 160)
    ("Cascadia Code"           . 160)
    ("Menlo"                   . 140)
    ("Liga SFMono Nerd Font"   . 140))
  "Fonts to try in order, as (FAMILY . HEIGHT).")

(defcustom shan/font-override nil
  "Runtime font choice as (FAMILY . HEIGHT), or nil to use the list.
Set by `shan/consult-font' and the size commands, and saved, so a choice
made at the keyboard survives a restart.  `shan/font-reset' clears it."
  :type '(choice (const :tag "Use shan/font-preferences" nil)
                 (cons string integer))
  :group 'shan)

(defun shan/font-choice (&optional frame)
  "Return (FAMILY . HEIGHT) to use: the override, else the first installed."
  (or shan/font-override
      (let ((installed (font-family-list frame)))
        (seq-find (lambda (pair) (member (car pair) installed))
                  shan/font-preferences))))

(defun shan/apply-font (&optional frame)
  "Set the default face from `shan/font-override' or `shan/font-preferences'.
Runs per FRAME rather than once at startup: under the daemon, init.el is
evaluated with no frame at all, so a bare `display-graphic-p' check is nil
and the font is silently never applied."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (let ((choice (shan/font-choice frame)))
        (if (null choice)
            (when (called-interactively-p 'interactive)
              (message "No font from shan/font-preferences is installed"))
          (set-face-attribute 'default frame
                              :family (car choice) :height (cdr choice)
                              :weight 'regular)
          ;; chrome stays put whatever the text does
          (dolist (face '(mode-line mode-line-inactive header-line))
            (set-face-attribute face frame :height shan/ui-font-height))
          (when (called-interactively-p 'interactive)
            (message "Font: %s %.1fpt%s" (car choice) (/ (cdr choice) 10.0)
                     (if shan/font-override " (saved)" ""))))))))

(defun shan/apply-font-all-frames (&rest _)
  "Apply the font to every frame.  Safe on any hook: extra args ignored."
  (dolist (f (frame-list)) (shan/apply-font f)))

(defun shan/font-set (family height)
  "Use FAMILY at HEIGHT everywhere, and remember it across restarts."
  (customize-save-variable 'shan/font-override (cons family height))
  (shan/apply-font-all-frames))

(defun shan/font-size-adjust (delta)
  "Change the text size by DELTA tenths of a point, everywhere, and save.
The mode line and header line do not move: they are pinned to
`shan/ui-font-height'."
  (interactive "p")
  (let* ((cur (or (shan/font-choice) (car shan/font-preferences)))
         (new (max 60 (+ (cdr cur) delta))))
    (shan/font-set (car cur) new)
    (message "Font: %s %.1fpt" (car cur) (/ new 10.0))))

(defun shan/font-bigger ()
  "Increase the text size by 1pt everywhere."
  (interactive) (shan/font-size-adjust 10))

(defun shan/font-smaller ()
  "Decrease the text size by 1pt everywhere."
  (interactive) (shan/font-size-adjust -10))

(defun shan/font-reset ()
  "Forget the saved font choice and go back to `shan/font-preferences'."
  (interactive)
  (customize-save-variable 'shan/font-override nil)
  (shan/apply-font-all-frames)
  (message "Font reset to shan/font-preferences"))

(add-hook 'after-make-frame-functions #'shan/apply-font)
;; A theme can reset face heights; re-assert ours afterwards.
(add-hook 'enable-theme-functions #'shan/apply-font-all-frames)
(shan/apply-font)   ; non-daemon startup, where a frame already exists


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
      ;; header-line inherits mode-line; pin it to the fixed chrome height.
      ;; It used to copy the *default* face height, so every text size
      ;; change dragged the header line along with it.
      (set-face-attribute 'header-line nil :height shan/ui-font-height)))
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


;;; ============================================================ Mode line

;; Smaller mode-line text without a shorter mode-line: a fixed height
;; shrinks the font (0.85 was *relative* to the default face, so the mode
;; line grew and shrank with every text size change; absolute holds still)
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
        (set-face-attribute face nil :height shan/ui-font-height)
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

;; `font-family-list' returns everything installed -- around 240 families
;; here, most of which make the preview look broken: proportional faces,
;; and icon/emoji fonts with no Latin glyphs at all.  Keep only families
;; that can actually render code.
(defun shan/usable-font-families (&optional frame)
  "Monospaced families in FRAME that have Latin glyphs.
Two tests, both cheap: `font-info' reports equal max and average width
only for a fixed-pitch font, and opening the font and asking for glyphs
for \"Agx\" rejects symbol/emoji families that are technically
fixed-pitch but render nothing readable."
  (let ((frame (or frame (selected-frame)))
        out)
    (dolist (family (delete-dups (font-family-list frame)))
      (ignore-errors
        (let* ((entity (find-font (font-spec :family family) frame))
               (info   (and entity (font-info entity frame))))
          ;; index 7 is max-width, 11 is average-width
          (when (and info (= (aref info 7) (aref info 11)))
            (let* ((object (open-font entity 14 frame))
                   (glyphs (and object (font-get-glyphs object 0 3 "Agx"))))
              (when (and glyphs (cl-every #'identity (append glyphs nil)))
                (push family out)))))))
    (sort out #'string<)))

;; --- Pick a font the way consult-theme picks a theme ------------------------
;; Live preview while you move through the list, original restored on C-g.
;; Height is kept from the current frame, so this changes family only --
;; sizing stays with `shan/font-preferences' / M-x shan/apply-font.
(defun shan/consult-font ()
  "Choose a font family with completion and live preview.
On RET the family is applied to all frames and saved, so it survives a
restart.  On abort the previous family is restored.  `shan/font-reset'
goes back to `shan/font-preferences'."
  (interactive)
  ;; require, not fboundp: consult--read is private and so carries no
  ;; autoload, while consult itself is deferred until one of its bound
  ;; commands runs.  Probing fboundp therefore failed on a fresh Emacs and
  ;; succeeded only after something else had already pulled consult in --
  ;; which reads exactly like the package being uninstalled.
  (unless (require 'consult nil t)
    (user-error "consult is not available"))
  (let* ((frame (or (seq-find #'display-graphic-p (frame-list))
                    (user-error "No graphical frame to preview in")))
         (original (face-attribute 'default :family frame))
         (families (shan/usable-font-families frame)))
    (consult--read
     families
     :prompt "Font: "
     :require-match t
     :category 'font
     :default original
     :history 'shan/consult-font-history
     :state
     (lambda (action candidate)
       (pcase action
         ;; preview as point moves; candidate is nil between candidates
         ('preview
           (when candidate
             (set-face-attribute 'default nil :family candidate)))
         ;; RET: keep it, and remember it across restarts
         ('return
           (when candidate
             (shan/font-set candidate (cdr (shan/font-choice frame)))
             (message "Font: %s (saved)" candidate)))
         ;; abort: put back what was there
         ('exit
          (unless candidate
            (set-face-attribute 'default nil :family original))))))))

(bind-key "H-F" #'shan/consult-font)
(define-key shan/hyper-map (kbd "F") #'shan/consult-font)

;; Text size, everywhere and remembered.  Deliberately not
;; `text-scale-increase': that is buffer-local, so it reverts in every other
;; buffer.  These move the default face instead, which the mode line and
;; header line no longer follow now that they are pinned.
(bind-key "H-+" #'shan/font-bigger)
(bind-key "H-=" #'shan/font-bigger)     ; same physical key without shift
(bind-key "H--" #'shan/font-smaller)
(bind-key "H-0" #'shan/font-reset)
(define-key shan/hyper-map (kbd "+") #'shan/font-bigger)
(define-key shan/hyper-map (kbd "=") #'shan/font-bigger)
(define-key shan/hyper-map (kbd "-") #'shan/font-smaller)
(define-key shan/hyper-map (kbd "0") #'shan/font-reset)
