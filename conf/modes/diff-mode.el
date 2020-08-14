(add-hook 'diff-mode-hook
          (lambda ()
            (setq should-delete-trailing-whitespace nil)))
