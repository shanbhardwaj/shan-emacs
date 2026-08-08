;;; llm-config.el --- LLM configuration -*- lexical-binding: t; -*-
(require 'use-package) ;; Ensure `use-package` is loaded

(use-package gptel
  :ensure t
  :custom
  ;; key is read from ~/.authinfo:
  ;;   machine api.openai.com login apikey password <your-key>
  (gptel-api-key #'gptel-api-key-from-auth-source)
  (gptel-model "gpt-4o")
  :bind
  (("C-c g q" . gptel-query)
   ("C-c g r" . gptel-response))
  :config
  (setq gptel-max-tokens 2048))

(use-package agent-shell
  :ensure t
  :commands (agent-shell-anthropic-start-claude-code
             agent-shell-openai-start-codex)
  :bind (("C-c a" . agent-shell-anthropic-start-claude-code)
         ("C-c A" . agent-shell-openai-start-codex))
  :config
  ;; both agents use subscription logins, not API keys:
  ;; claude auth for Claude Code, ChatGPT login (codex login) for Codex
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t))

  ;; VS Code-inspired rendering: tinted panels for user input and code,
  ;; monospace inline code inside proportional prose.  Backgrounds are
  ;; derived from the current theme, so this follows auto-dark switches.
  (defun shan/style-agent-shell-faces (&rest _)
    "Give agent-shell markdown faces subtle, theme-relative panels."
    (when (facep 'agent-shell-markdown-inline-code)
      (let* ((bg (face-attribute 'default :background nil t))
             (dark (eq (frame-parameter nil 'background-mode) 'dark))
             (tint (if (and (stringp bg) (string-prefix-p "#" bg))
                       (if dark (color-lighten-name bg 8)
                         (color-darken-name bg 4))
                       'unspecified)))
        ;; inline code: monospace island in the prose
        (set-face-attribute 'agent-shell-markdown-inline-code nil
                            :background tint :inherit 'fixed-pitch)
        ;; code blocks and the user's own input: full-width panels
        (dolist (face '(agent-shell-markdown-source-block agent-shell-input))
          (when (facep face)
            (set-face-attribute face nil :background tint :extend t)))
        ;; headers stand out without shouting
        (dolist (n '(1 2 3))
          (let ((face (intern (format "agent-shell-markdown-header-%d" n))))
            (when (facep face)
              (set-face-attribute face nil :weight 'bold)))))))
  (add-hook 'enable-theme-functions #'shan/style-agent-shell-faces)
  (shan/style-agent-shell-faces))

(use-package agent-shell-dashboard
  ;; not on MELPA; linked from the official agent-shell README
  :vc (:url "https://github.com/wandersoncferreira/agent-shell-dashboard" :rev :newest)
  :after agent-shell
  :commands (agent-shell-dashboard)
  :bind ("C-c d" . agent-shell-dashboard)
  :config
  (setq agent-shell-dashboard-summary-command
        '("claude" "-p" "--allowed-tools" "" "--model" "haiku"))
  (setq agent-shell-dashboard-recent-sessions-count 8)

  ;; Claude Code keeps a live status registry at ~/.claude/sessions/<pid>.json,
  ;; rewritten by the REPL on every state transition.  That is a better
  ;; "is it working / does it need me" signal than hooks: the Notification
  ;; hook only fires ~6s after a permission prompt opens (and not at all if
  ;; you answer it quickly), while this file is always current.
  (defun my/claude-live-sessions ()
    "Live Claude Code sessions from the CLI's own status registry.
Each entry is a plist (:id :pid :cwd :status :waiting-for :name).
STATUS is busy | waiting | idle | shell, or \"unknown\" for sessions
whose CLI is too old to record one."
    (let ((dir (expand-file-name "~/.claude/sessions/"))
          out)
      (when (file-directory-p dir)
        (dolist (f (directory-files dir t "\\.json\\'"))
          (ignore-errors
            (let* ((json (with-temp-buffer
                           (insert-file-contents f)
                           (json-parse-buffer :object-type 'alist
                                              :null-object nil
                                              :false-object nil)))
                   (pid (alist-get 'pid json))
                   (attrs (and (integerp pid) (process-attributes pid))))
              ;; live only: pid exists AND is actually a claude process
              ;; (cheap guard against pid reuse)
              (when (and attrs
                         (string-match-p "claude"
                                         (or (alist-get 'args attrs) "")))
                (push (list :id (alist-get 'sessionId json)
                            :pid pid
                            :cwd (alist-get 'cwd json)
                            :status (or (alist-get 'status json) "unknown")
                            :waiting-for (alist-get 'waitingFor json)
                            :name (alist-get 'name json)
                            ;; ms epoch -> seconds, for sorting with transcripts
                            :time (/ (or (alist-get 'statusUpdatedAt json)
                                         (alist-get 'updatedAt json)
                                         0)
                                     1000.0))
                      out))))))
      (nreverse out)))

  (defun my/claude-session-status (id)
    "Return (STATUS . WAITING-FOR) for session ID, or nil if not live."
    (when-let* ((s (seq-find (lambda (s) (equal (plist-get s :id) id))
                             (my/claude-live-sessions))))
      (cons (plist-get s :status) (plist-get s :waiting-for))))

  (defun my/claude-cli-session-cwd (file)
    "Return the cwd recorded in Claude Code transcript FILE, or nil."
    (with-temp-buffer
      (insert-file-contents file nil 0 65536)
      (goto-char (point-min))
      (when (re-search-forward "\"cwd\":\"\\([^\"]+\\)\"" nil t)
        (match-string 1))))

  (defun my/agent-shell-dashboard-recent-sessions ()
    "Recent sessions from agent-shell transcripts plus the Claude CLI store.
Merges the dashboard's default source with sessions found under
~/.claude/projects/ so sessions started in other terminals show up too."
    (let* ((defaults (append (my/claude-live-sessions)   ; live always listed
                             (agent-shell-dashboard--recent-sessions-default)))
           (live (agent-shell-dashboard--live-session-ids))
           (seen (make-hash-table :test 'equal))
           (root (expand-file-name "~/.claude/projects/"))
           ;; UUID-named files only; skips agent-*.jsonl subagent transcripts.
           (files (and (file-directory-p root)
                       (directory-files-recursively
                        root
                        "\\`[0-9a-f]\\{8\\}\\(?:-[0-9a-f]\\{4\\}\\)\\{3\\}-[0-9a-f]\\{12\\}\\.jsonl\\'")))
           (out (copy-sequence defaults)))
      (dolist (s defaults)
        (puthash (plist-get s :id) t seen))
      ;; Newest files first; only parse a handful to keep refresh cheap.
      (setq files (seq-take
                   (seq-sort-by
                    (lambda (f)
                      (float-time (file-attribute-modification-time
                                   (file-attributes f))))
                    #'> files)
                   25))
      (dolist (f files)
        (let ((id (file-name-base f))
              (mtime (float-time (file-attribute-modification-time
                                  (file-attributes f)))))
          (unless (or (gethash id seen) (gethash id live))
            (when-let* ((cwd (my/claude-cli-session-cwd f)))
              (puthash id t seen)
              (push (list :id id :cwd cwd :agent "Claude Code"
                          :opened mtime :time mtime)
                    out)))))
      ;; live sessions are never trimmed; the count caps the historical tail
      (let* ((sorted (seq-sort-by (lambda (s) (or (plist-get s :time) 0)) #'> out))
             (live-rows (seq-filter (lambda (s) (plist-get s :status)) sorted))
             (rest (seq-remove (lambda (s) (plist-get s :status)) sorted)))
        (append live-rows
                (seq-take rest agent-shell-dashboard-recent-sessions-count)))))

  (setq agent-shell-dashboard-recent-sessions-function
        #'my/agent-shell-dashboard-recent-sessions)

  ;; RET on a recent session: the dashboard's default reopens it as an
  ;; agent-shell buffer, but these sessions are claude-code-ide ones (they
  ;; come from ~/.claude/projects/), so send them back to that client.
  ;; NOTE: claude-code-ide's own `session-id' is an MCP identifier, not the
  ;; CLI's -- and its `-r' takes no argument -- so an exact resume has to go
  ;; through --resume in the extra-flags string.
  (defun my/dashboard-open-claude-code-session (session)
    "Open SESSION in claude-code-ide: switch if live, else resume by id."
    (let* ((cwd (plist-get session :cwd))
           (id (plist-get session :id))
           (default-directory (file-name-as-directory
                               (or cwd default-directory))))
      (if-let* ((buf (get-buffer (claude-code-ide--get-buffer-name))))
          (claude-code-ide--display-buffer-in-side-window buf)
        (let ((claude-code-ide-cli-extra-flags
               (string-trim
                (format "%s --resume %s"
                        (or claude-code-ide-cli-extra-flags "") id))))
          (claude-code-ide)))))

  (setq agent-shell-dashboard-resume-recent-function
        #'my/dashboard-open-claude-code-session)

  ;; --- status in the row label -------------------------------------------
  ;; glyph AND word, so the state does not depend on colour alone
  (defface my/dashboard-busy '((t :inherit success :weight bold))
    "Session actively working.")
  (defface my/dashboard-waiting '((t :inherit warning :weight bold))
    "Session waiting for user input.")
  (defface my/dashboard-idle '((t :inherit shadow))
    "Session live but idle.")

  (defun my/dashboard-status-suffix (session)
    "Return a propertized \" ● working\"-style suffix for SESSION, or \"\"."
    (pcase (plist-get session :status)
      ("busy"    (propertize "  ● working" 'face 'my/dashboard-busy))
      ("waiting" (propertize (format "  ▲ needs input%s"
                                     (if-let* ((w (plist-get session :waiting-for)))
                                         (format " — %s" w) ""))
                             'face 'my/dashboard-waiting))
      ("idle"    (propertize "  ○ idle" 'face 'my/dashboard-idle))
      ("shell"   (propertize "  ○ shell" 'face 'my/dashboard-idle))
      ("unknown" (propertize "  ? unknown" 'face 'my/dashboard-idle))
      (_ "")))

  (defun my/dashboard-session-name (session)
    "Label for SESSION: project directory plus its live status."
    (let ((base (or (plist-get session :name)
                    (when-let* ((cwd (plist-get session :cwd)))
                      (file-name-nondirectory (directory-file-name cwd)))
                    "session")))
      (concat base (my/dashboard-status-suffix session))))

  (setq agent-shell-dashboard-session-name-function
        #'my/dashboard-session-name)

  ;; --- refresh while the dashboard is on screen --------------------------
  (defvar my/dashboard-refresh-timer nil)

  (defun my/dashboard-refresh-visible ()
    "Re-render the dashboard when it is displayed; otherwise stop the timer."
    (let ((buf (seq-find (lambda (b)
                           (with-current-buffer b
                             (derived-mode-p 'agent-shell-dashboard-mode)))
                         (buffer-list))))
      (if (and buf (get-buffer-window buf t))
          (with-current-buffer buf
            (ignore-errors (revert-buffer nil t)))
        (when my/dashboard-refresh-timer
          (cancel-timer my/dashboard-refresh-timer)
          (setq my/dashboard-refresh-timer nil)))))

  (defun my/dashboard-start-refresh (&rest _)
    (unless my/dashboard-refresh-timer
      (setq my/dashboard-refresh-timer
            (run-with-timer 5 5 #'my/dashboard-refresh-visible))))
  (advice-add 'agent-shell-dashboard :after #'my/dashboard-start-refresh))

;; terminal emulator backed by libghostty; the native module lives in
;; var/ghostel/ (outside the package dir) so package updates can't
;; clobber it while loaded
(use-package ghostel
  :ensure t
  :defer t
  :custom
  (ghostel-module-directory (expand-file-name "var/ghostel/" user-emacs-directory))
  (ghostel-module-auto-install 'download)
  ;; defaults plus M-o: keep window-hopping working inside terminals
  (ghostel-keymap-exceptions
   '("C-c" "C-x" "C-u" "C-h" "M-x" "M-:" "C-\\" "M-o"))
  :config
  ;; TUIs that auto-pick a light/dark palette (Claude Code with theme
  ;; "auto") ask the terminal for its background via OSC 11.  ghostel
  ;; answers from `ghostel-default', but when that face has no explicit
  ;; background it resolves against whichever frame is selected -- and a
  ;; TTY frame yields ghostel's #000000 fallback, which would report
  ;; "dark" on a light theme.  Pin the colours so the reply is always
  ;; the real theme background.  Visually neutral: same colour the face
  ;; already inherits.  ghostel re-pushes these on theme change via its
  ;; own `enable-theme-functions' hook (ghostel-sync-theme).
  (defun shan/ghostel-pin-default-colors (&rest _)
    "Pin `ghostel-default' colours so OSC 10/11 replies are deterministic."
    (let ((bg (face-attribute 'default :background nil t))
          (fg (face-attribute 'default :foreground nil t)))
      (when (and (stringp bg) (string-prefix-p "#" bg))
        (set-face-attribute 'ghostel-default nil
                            :background bg :foreground fg))))
  (add-hook 'enable-theme-functions #'shan/ghostel-pin-default-colors)
  (shan/ghostel-pin-default-colors))

;; same exemption for any remaining eat terminal buffers
(with-eval-after-load 'eat
  (define-key eat-semi-char-mode-map (kbd "M-o") #'other-window))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup) ; expose Emacs MCP tools (xref, imenu, ...)
  (setq claude-code-ide-terminal-backend 'ghostel))

;;; --- Claude Code tool-call highlighting -----------------------------------
;;
;; Bands tool-call blocks so they read as sections instead of a wall of
;; text.  Overlays are the only option here: ghostel's native renderer
;; writes anonymous face plists as text properties, so face-remap and
;; font-lock have nothing to hook (and font-lock is disabled in the mode).
;;
;; Claude Code marks blocks with coloured glyphs, and the colour is the
;; discriminator -- the same marker is used for assistant prose:
;;   green/red/grey  U+23FA  tool call (ok / errored / collapsed)
;;   white           U+23FA  prose -- must NOT be banded
;;   grey            U+23BF  tool output
;; Those are hardcoded truecolor from the CLI, so they can drift on a CLI
;; upgrade; that is what the two colour defcustoms are for.

(require 'color)

(defcustom shan/cc-header-colors
  '("#4eba65" "#ff6b80" "#999999"    ; dark theme:  success / error / inactive
    "#2c7a39" "#ab2b3f" "#666666")   ; light theme: success / error / inactive
  "Foregrounds of the tool-call marker, for both Claude Code palettes.
Taken from the CLI's own theme tables, so both are listed and the bands
survive a light/dark switch.  The body-text colours are deliberately
absent -- prose uses the same marker glyph and must not be banded
\(#ffffff in the dark palette, #000000 in the light one), as is the
warning colour used for system notices."
  :type '(repeat string) :group 'tools)

(defcustom shan/cc-output-colors '("#999999" "#666666")
  "Foregrounds of the tool-output marker (dark and light palettes)."
  :type '(repeat string) :group 'tools)

(defface shan/cc-header '((t :extend t)) "Tool-call header band.")
(defface shan/cc-output '((t :extend t)) "Tool-output band.")

(defun shan/cc-update-faces (&rest _)
  "Derive band colours from the current theme (follows auto-dark)."
  (let ((bg (face-attribute 'default :background nil t)))
    (when (and (stringp bg) (string-prefix-p "#" bg))
      (let ((dark (eq (frame-parameter nil 'background-mode) 'dark)))
        (set-face-attribute 'shan/cc-header nil :extend t
                            :background (if dark (color-lighten-name bg 7)
                                          (color-darken-name bg 5)))
        (set-face-attribute 'shan/cc-output nil :extend t
                            :background (if dark (color-lighten-name bg 3)
                                          (color-darken-name bg 2)))))))
(add-hook 'enable-theme-functions #'shan/cc-update-faces)
(shan/cc-update-faces)

(defsubst shan/cc--fg (pos)
  (let ((f (get-text-property pos 'face)))
    (and (consp f) (plist-get f :foreground))))

(defun shan/cc--output-line-p (bol eol)
  "Non-nil if this line starts a tool-output row (marker within 8 columns)."
  (let ((p bol) (limit (min eol (+ bol 8))) hit)
    (while (and (< p limit) (not hit))
      (when (and (eq (char-after p) ?⎿)
                 (member (shan/cc--fg p) shan/cc-output-colors))
        (setq hit t))
      (setq p (1+ p)))
    hit))

(defun shan/cc--set-line (bol eol face)
  "Make line BOL..EOL carry FACE, reusing any existing overlay.
Reuse matters: recreating overlays on every repaint would churn
redisplay.  `evaporate' matters too -- the renderer repaints by
delete-region + insert, which collapses overlays it covers, and without
it they would pile up at point-min."
  (let* ((end (min (point-max) (1+ eol)))
         (ov (seq-find (lambda (o) (overlay-get o 'shan/cc))
                       (overlays-in bol (min (point-max) (1+ bol))))))
    (cond
     ((null face) (when ov (delete-overlay ov)))
     ((and ov (eq (overlay-get ov 'face) face) (eq (overlay-end ov) end)) nil)
     (ov (move-overlay ov bol end) (overlay-put ov 'face face))
     (t (let ((new (make-overlay bol end nil t nil)))
          (overlay-put new 'shan/cc t)
          (overlay-put new 'face face)
          (overlay-put new 'evaporate t))))))

(defun shan/cc--scan-visible ()
  "Band tool-call blocks across the visible region of each window.
A block opens at a coloured header marker, switches to output at the
first output marker, and closes on a blank line -- no indent heuristics,
so wrapped header lines and varying output indents both work."
  (with-demoted-errors "claude-code highlight: %S"
    (dolist (w (get-buffer-window-list (current-buffer) nil t))
      (save-excursion
        ;; back up to a blank line or header so block state is unambiguous
        (goto-char (window-start w))
        (forward-line 0)
        (let ((stop (max (point-min) (- (point) 8000))))
          (when (re-search-backward "^⏺\\|^$" stop t) (forward-line 0)))
        (let ((end (save-excursion (goto-char (window-start w))
                                   (forward-line (+ (window-body-height w) 10))
                                   (point)))
              (state nil) (n 0))
          (while (and (< (point) end) (< n 400))
            (setq n (1+ n))
            (let* ((bol (point)) (eol (line-end-position)) face)
              (cond
               ((eq bol eol) (setq state nil))            ; blank closes block
               ((eq (char-after bol) ?⏺)
                (setq state (and (member (shan/cc--fg bol) shan/cc-header-colors)
                                 'header)))
               ((shan/cc--output-line-p bol eol) (setq state 'output)))
              (setq face (pcase state
                           ('header 'shan/cc-header)
                           ('output 'shan/cc-output)))
              ;; never band a row the CLI already gave a background (diff
              ;; hunks): an overlay face would erase that colouring
              (when (and face (get-text-property bol 'face)
                         (plist-get (get-text-property bol 'face) :background))
                (setq face nil))
              (shan/cc--set-line bol eol face))
            (forward-line 1))
          (shan/cc--prune (window-start w) end))))))

(defun shan/cc--prune (beg end)
  "Drop our overlays far outside BEG..END once they pile up.
Scrolling through a long session keeps creating bands in scrollback;
they are cheap but unbounded, and scrolling back re-creates them."
  (let ((ours (seq-filter (lambda (o) (overlay-get o 'shan/cc))
                          (overlays-in (point-min) (point-max)))))
    (when (> (length ours) 600)
      (let ((lo (max (point-min) (- beg 20000)))
            (hi (min (point-max) (+ end 20000))))
        (dolist (o ours)
          (when (or (< (overlay-end o) lo) (> (overlay-start o) hi))
            (delete-overlay o)))))))

(defun shan/cc--after-redraw (buffer &optional _force)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (bound-and-true-p shan/cc-highlight-mode)
        (shan/cc--scan-visible)))))

(defun shan/cc--on-scroll (window _start)
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when (bound-and-true-p shan/cc-highlight-mode)
        (shan/cc--scan-visible)))))

(define-minor-mode shan/cc-highlight-mode
  "Band Claude Code tool-call blocks in this ghostel buffer."
  :lighter " CC"
  (if shan/cc-highlight-mode
      (progn
        (add-hook 'window-scroll-functions #'shan/cc--on-scroll nil t)
        (shan/cc--scan-visible))
    (remove-hook 'window-scroll-functions #'shan/cc--on-scroll t)
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (overlay-get o 'shan/cc) (delete-overlay o)))))

(defun shan/cc-maybe-enable ()
  "Enable in claude-code-ide session buffers only, not other terminals.
Also applies the reading tweaks (spacing, slightly smaller text) here
rather than globally, so plain ghostel terminals keep their defaults."
  (when (string-prefix-p "*claude-code[" (buffer-name))
    (setq-local line-spacing 0.1)
    (face-remap-add-relative 'default :height 0.95)
    (shan/cc-highlight-mode 1)))

(add-hook 'ghostel-mode-hook #'shan/cc-maybe-enable)
(with-eval-after-load 'ghostel
  (advice-add 'ghostel--redraw-now :after #'shan/cc--after-redraw))

(provide 'llm-config)
