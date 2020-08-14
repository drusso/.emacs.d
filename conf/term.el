(add-hook 'term-exec-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


