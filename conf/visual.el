;; Set the font size to 10pt. Note for presentations, 16pt is a good size.
(set-face-attribute 'default nil :height 100)

(load-theme 'molokai t)

;; Highlight matching parentheses, etc.
(show-paren-mode 1)

;; Turn on line numbers.
(global-display-line-numbers-mode)
(set-face-attribute 'line-number-current-line nil :foreground "#66D9EF")

;; Make the left fringe visible and the right fringe invisible. The left fringe
;; is used for the git line status (diff-hl). Note that since the right fringe
;; is invisibile, when lines run longer than the window's width, emacs will
;; render a `->` at the end of the line.
(setq-default left-fringe-width 3)
(setq-default right-fringe-width 0)

;; The left-fringe-width and right-fringe-width's set above do not apply to the
;; *scratch* buffer at boot. This will fix it.
(set-window-fringes (get-buffer-window) 3 0)

;; Turn on diff-hl-mode everywhere.
(global-diff-hl-mode)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Customization for the highlight-indent-guides mode.
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-responsive 'stack)
(setq highlight-indent-guides-auto-character-face-perc 4)


