;; Display magit buffers in the current window, rather than a new window.
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer buffer '(display-buffer-same-window))))

;; Don't show the diff buffer when authoring a commit. To manually bring up the
;; diff, use C-c C-d.
(remove-hook 'server-switch-hook 'magit-commit-diff)
