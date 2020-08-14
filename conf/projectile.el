(projectile-global-mode)

(setq projectile-require-project-root t)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching nil)
(setq projectile-use-git-grep t)
(setq projectile-globally-ignored-directories
      (append '(
                ;; Include a list of directory names that projectile will
                ;; ignore globally.
                )
              projectile-globally-ignored-directories))

(defun switch-to-project-buffer-or-find-file (&optional arg)
  "Implements a projectile switch project action. Switches to any
open buffer in the current project; if none is open, present the
open file prompt. This differs from projectile's default
behaviour, which will always presents the open file prompt."
  (let ((buffer-candidates (delete (buffer-name (current-buffer))
                                   (projectile-project-buffer-names))))
    (if (> (length buffer-candidates) 0)
        (switch-to-buffer (car buffer-candidates))
        (projectile-find-file) ;; Present the open file prompt.
      )
    )
  )

;; Override projectile's default behaviour when switching projects (i.e. with
;; projectile-switch-project or projectile-switch-open-project).
(setq projectile-switch-project-action 'switch-to-project-buffer-or-find-file)
