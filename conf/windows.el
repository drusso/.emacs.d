;; Note there are some magit-specific window split overrides in
;; modes/magit-mode.el.

;; When emacs splits a window, prefer a horizontal stack rather than a vertical
;; stack.
(setq split-width-threshold 80)
(setq split-height-threshold nil)

;; Open buffers in the same window (if they match).
(setq same-window-regexps
      '("*.+*" ;; ex: *Help*
        ".*"
        )
      )
