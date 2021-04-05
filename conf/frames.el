;; Set the frame title to the current tab # and name.
(setq frame-title-format
      (list
       '(:eval (dr/current-tab-name-with-index))))

(defun turn-off-bars ()
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

;; Remove the toolbar.
(turn-off-bars)

;; Remove tooltips.
(tooltip-mode -1)

;; Remove the toolbar in new frames.
(add-hook 'before-make-frame-hook 'turn-off-bars)

(defun maximize-frame ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'maximized))

(defun make-frame-big (frame)
  (let ((x (x-display-pixel-width))
        (y (x-display-pixel-height)))
    (set-frame-parameter frame 'width (truncate (/ x 8)))
    (set-frame-parameter frame 'height (truncate (/ y 15)))
    (set-frame-parameter frame 'left 175)
    (set-frame-parameter frame 'top 150)
    ))

;; Maximize the frame on startup.
(maximize-frame)

;; Make new frames (not the one on startup) large.
(add-hook 'after-make-frame-functions 'make-frame-big)
