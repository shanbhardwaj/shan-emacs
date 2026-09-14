;;; host-shan-4090.el --- Machine-specific settings for shan-4090 -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded by init.el BEFORE the lisp/ modules, and only on this host.  See the
;; "Per-machine settings" block there for the loading rule.
;;
;; Everything here is a plain `setq' of a variable that a later module declares
;; with `defvar' or `defcustom'.  Both leave an already-bound value alone, so
;; setting it first wins without any override machinery: the module's own value
;; becomes the fallback for machines that have no host file.
;;
;; What belongs here: anything true of this DISPLAY or this BOX rather than of
;; the configuration.  What does not: preferences you would want on the Mac too.

;;; Code:

;; --- Typography -------------------------------------------------------------
;; This is a 27" 5120x2880 panel at ~218 PPI, run by Hyprland at integer scale
;; 2.  The Mac's screen is a different size and wants different numbers, and
;; the shared `shan/font-preferences' in init-ui.el could not express that:
;; it keys the height off which FAMILY is installed, and once both machines
;; had the same families installed there was nothing left to distinguish them.
;; Hence this file.
;;
;; 80 = 8pt.  Glyphs rasterise at `size * scale', so 8 * 2 = 16 physical
;; pixels, a whole number, which is what lets hinting sit on the pixel grid.
;; Keep this on a whole or half point while the compositor is at scale 2.
(setq shan/font-preferences
      '(("Noto Sans Mono"          . 100)
        ("FiraCode Nerd Font Mono" . 100)))

;; The Mac's saved font lives in the shared custom.el, and a saved choice
;; outranks the list above, so this machine would show the Mac's 14pt text.
;; This file loads after custom.el, so clearing the saved choice here keeps
;; shan-4090 on its own sizes; the Mac keeps its choice.
(setq shan/font-override nil)

;; The mode line and header line.  init-ui.el pins these to an ABSOLUTE height
;; on purpose, so that `shan/font-bigger' and friends do not drag the chrome
;; around with the text.  That is still the right behaviour, but it means the
;; constant silently encodes an assumption about the default face: the shared
;; value of 120 is 0.85 * 140, and 140 was the height back when CaskaydiaMono
;; was the chosen family.
;;
;; With the text at 80 that left the mode line 45% LARGER than the body text,
;; which inverts the intent -- the 0.85 factor exists to make the chrome
;; smaller.  68 is 0.85 * 80, restoring the ratio at this machine's size.
;;
;; If the text height above changes, change this too: nothing recomputes it.
(setq shan/ui-font-height 80)

(provide 'host-shan-4090)
;;; host-shan-4090.el ends here
