;; This is a built-in. Check for mode-specific overrides.
(setq-default show-trailing-whitespace t)

;; This is a built-in. Do not indicate unused buffer lines.
(setq-default indicate-empty-lines nil)

;; This is a built-in. Check for mode-specific overrides.
(setq-default require-final-newline t)

;; This is a built-in.
(setq-default indent-tabs-mode nil)

;; This is a custom flag for the hook below.
(setq-default should-delete-trailing-whitespace t)

;; Automatically delete trailing whitespace on file save.
(add-hook 'write-file-hooks
          (lambda ()
            (if should-delete-trailing-whitespace
                (delete-trailing-whitespace))))
