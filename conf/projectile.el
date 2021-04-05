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

(counsel-projectile-mode t)
(setq counsel-projectile-remove-current-project t)

;; When using projectile commands directly, rather than the counsel variants,
;; use ivy completion.
(setq projectile-completion-system 'ivy)

(defun dr/counsel-projectile-switch-open-project ()
  "A counsel version of `projectile-switch-open-project', in lieu
of one from the counsel library."
  (interactive)
  (let ((dr/projectile-force-open-projects t))
    (counsel-projectile-switch-project)))

(advice-add
 'projectile-relevant-known-projects
 :around
 (lambda (fn &rest args)
   (if (and (boundp 'dr/projectile-force-open-projects)
            dr/projectile-force-open-projects)
       (apply (symbol-function 'projectile-relevant-open-projects) args)
       (apply fn args))))

(defun dr/counsel-projectile-switch-project-action-maybe-vc (project-root)
  "Open a project to magit (or any other supported VCS), with a
fallback to dired if the project is not managed with a VCS."
  (let ((vc (projectile-project-vcs project-root)))
    (if (eq vc 'none)
        (counsel-projectile-switch-project-action-dired project-root)
      (counsel-projectile-switch-project-action-vc project-root))))
