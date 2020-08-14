;; Enable mouse support in terminal.
;;
;; Note `window-system` is `nil` when in a terminal, and "ns" when in a Cocoa
;; frame.
(if (not window-system)
    (progn
      (xterm-mouse-mode t)
      (global-set-key [mouse-4]
       '(lambda ()
          (interactive)
          (scroll-down 1)))
      (global-set-key [mouse-5]
       '(lambda ()
          (interactive)
          (scroll-up 1)))
      (defun track-mouse (e))
      (setq mouse-sel-mode t)))

;; Do not scroll dramatically when moving one line at a time.
(setq-default scroll-conservatively 1)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5)))
