;; Customize the mode line. Uses smart-mode-line and rich-minority.
(sml/setup)
(rich-minority-mode 1)

;; This empty list will stop smart-mode-line from making any space-saving
;; replacements of the buffer name.
(setq sml/replacer-regexp-list '())

;; This empty string will hide *every* minor mode.
(setq rm-excluded-modes "")

;; Show column numbers in the mode line.
(setq column-number-mode t)
