(add-hook 'git-rebase-mode-hook
          (lambda ()
            (setq should-delete-trailing-whitespace nil)))
