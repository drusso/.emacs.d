;; Set the font size to 10pt. Note for presentations, 16pt is a good size.
(set-face-attribute 'default nil :height 100)

(load-theme 'molokai t)

;; Highlight matching parentheses, etc.
(show-paren-mode 1)

;; Turn on line numbers.
(global-display-line-numbers-mode)
(set-face-attribute 'line-number-current-line nil :foreground "#66D9EF")

;; Add and customize a divider between windows.
(setq window-divider-default-right-width 1)
(set-face-foreground 'window-divider "black")
(window-divider-mode)

;; Turn on diff-hl-mode everywhere.
(global-diff-hl-mode)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Customization for the highlight-indent-guides mode.
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-responsive 'stack)
(setq highlight-indent-guides-auto-character-face-perc 4)


