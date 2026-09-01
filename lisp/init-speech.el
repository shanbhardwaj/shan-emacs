;;; init-speech.el --- Read buffer text aloud -*- lexical-binding: t; -*-

;;; Commentary:

;; Speech synthesis on both machines, using what each already ships rather
;; than a package: `say' on macOS, `spd-say' on Linux (espeak-ng is the
;; fallback there).  Nothing to install on either.
;;
;;   C-c t   speak the region, or from point to the end of the buffer
;;   C-c T   stop
;;
;; Only one utterance runs at a time.  Speaking again stops whatever was
;; already talking rather than layering a second voice over the first, which
;; is what makes C-c t usable as "read from here" while moving around.
;;
;; Deliberately not emacspeak: that is a whole audio desktop that speaks every
;; interaction.  This is the narrow thing -- read this text to me.

;;; Code:

(defgroup shan/speech nil
  "Read buffer text aloud."
  :group 'external)

(defcustom shan/speech-rate nil
  "Words per minute, or nil for the system default.
macOS `say' and Linux `spd-say' both take a rate, but on different
scales, so this is translated per platform in `shan/speech--command'."
  :type '(choice (const :tag "System default" nil) integer)
  :group 'shan/speech)

(defcustom shan/speech-voice nil
  "Voice name, or nil for the system default.
Run `say -v \"?\"' on macOS or `spd-say -L' on Linux to list them."
  :type '(choice (const :tag "System default" nil) string)
  :group 'shan/speech)

(defvar shan/speech--process nil
  "The running speech process, if any.")

(defun shan/speech--command (text)
  "Return the command list that speaks TEXT on this system."
  (cond
   ((eq system-type 'darwin)
    (append '("say")
            (when shan/speech-rate (list "-r" (number-to-string shan/speech-rate)))
            (when shan/speech-voice (list "-v" shan/speech-voice))
            (list text)))
   ((executable-find "spd-say")
    ;; spd-say takes -r as -100..100 rather than words per minute, so a rate
    ;; given in wpm is mapped roughly onto that range around a 175 wpm centre.
    (append '("spd-say" "--wait")
            (when shan/speech-rate
              (list "-r" (number-to-string
                          (max -100 (min 100 (/ (- shan/speech-rate 175) 2))))))
            (when shan/speech-voice (list "-y" shan/speech-voice))
            (list text)))
   ((executable-find "espeak-ng")
    (append '("espeak-ng")
            (when shan/speech-rate (list "-s" (number-to-string shan/speech-rate)))
            (list text)))
   (t (user-error "No speech program found (say, spd-say or espeak-ng)"))))

(defun shan/speech-stop ()
  "Stop speaking."
  (interactive)
  (when (process-live-p shan/speech--process)
    (delete-process shan/speech--process))
  (setq shan/speech--process nil)
  (when (called-interactively-p 'interactive)
    (message "Stopped")))

(defun shan/speech-speak (start end)
  "Speak the text between START and END.
Interactively that is the region, or from point to the end of the
buffer when nothing is selected -- so this reads on from wherever you
are rather than starting over."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point-max))))
  (let ((text (string-trim (buffer-substring-no-properties start end))))
    (when (string-empty-p text)
      (user-error "Nothing to speak"))
    ;; Replace rather than layer: a second voice over the first is never what
    ;; is wanted, and there is no way to tell them apart once both are going.
    (shan/speech-stop)
    (setq shan/speech--process
          (make-process
           :name "shan-speech"
           :buffer nil                  ; no output worth keeping
           :command (shan/speech--command text)
           :noquery t                   ; never block exiting Emacs
           :sentinel (lambda (_p event)
                       (when (string-match-p "\\`\\(finished\\|exited\\)" event)
                         (setq shan/speech--process nil)))))
    (message "Speaking %d characters. C-c T to stop." (length text))))

(bind-key "C-c t" #'shan/speech-speak)
(bind-key "C-c T" #'shan/speech-stop)

(provide 'init-speech)
;;; init-speech.el ends here
