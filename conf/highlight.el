(require 'hi-lock)

(defun dr/highlight ()
  "Toggles highlighting for the symbol at the point. If the region
is active, adds a highlight for the region instead."
  (interactive)
  (if (region-active-p)
      (let ((str (buffer-substring (region-beginning) (region-end)))
            ;; This flag directs `hi-lock-read-face-name' to auto-select the
            ;; next face (instead of prompting).
            (hi-lock-auto-select-face t)
            (face (hi-lock-read-face-name)))
          (hi-lock-face-phrase-buffer str face))
    (let ((highlights (hi-lock--regexps-at-point)))
      (if highlights
          (hi-lock-unface-buffer (car highlights))
        (hi-lock-face-symbol-at-point)))))

(defun dr/highlight-remove-all ()
  "Remove all highlights from the current buffer."
  (interactive)
  (hi-lock-unface-buffer t))
