;; Set the font size to 10pt. Note for presentations, 16pt is a good size.
(set-face-attribute 'default nil :height 100)

(load-theme 'badger t)

;; Customize the colours.
(set-face-attribute 'default nil
                    :background "#0D0D12"
                    )
(set-face-attribute 'region nil
                    :background "#4371A1"
                    )
(set-face-attribute 'fringe nil
                    :background "#15151F"
                    )
(set-face-attribute 'window-divider nil
                    :foreground "#363338"
                    )
(set-face-attribute 'font-lock-doc-face nil
                    :foreground "#E6DB74"
                    )
(set-face-attribute 'font-lock-string-face nil
                    :foreground "#E6DB74"
                    )
(set-face-attribute 'font-lock-constant-face nil
                    :foreground "#86B187"
                    )
(set-face-attribute 'font-lock-preprocessor-face nil
                    :foreground "wheat"
                    )
(set-face-attribute 'font-lock-type-face nil
                    :foreground "#D4AED4"
                    )

;; Blink the cursor.
(blink-cursor-mode t)

;; Highlight matching parentheses, etc.
(show-paren-mode 1)

;; Turn on line numbers.
(global-display-line-numbers-mode)
(set-face-attribute 'line-number-current-line nil
                    :weight 'bold
                    :foreground "#F28B86"
                    :background "#0D0D15"
                    )

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
