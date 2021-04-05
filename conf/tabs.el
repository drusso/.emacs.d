;; Disable the tab bar (not yet supported on macOS --with-ns builds). This does
;; not disable tabs, only the visual bar.
(setq tab-bar-show nil)

;; On initialization, name the initial tab.
(tab-rename "scratch")

(defun dr/new-tab ()
  "Create a new tab, prompting for its name first."
  (interactive)
  (let* ((name (read-from-minibuffer "New tab: "))
         (tab (tab-new)))
    (if name (tab-rename name))))

(defun dr/current-tab-index ()
  "Returns the tab index prefixed with # and 1-indexed."
  (format "#%d"
          (+ (tab-bar--current-tab-index) 1)))

(defun dr/current-tab-name-with-index ()
  "Returns the tab index (prefixed with # and 1-indexed) and the
name of the tab."
  (format "%s - %s"
          (dr/current-tab-index)
          (alist-get 'name (tab-bar--current-tab))))

(defun dr/open-project-in-new-tab-action (project-root)
  "Opens a project in a new tab. The project opens immediately to
the project's magit status buffer (or equivalent for other
VCS). The tab is named after the project name."
  (tab-new)
  (dr/counsel-projectile-switch-project-action-maybe-vc project-root)
  (let ((name (projectile-project-name)))
    (tab-rename name)))

(defun dr/open-project-in-new-tab ()
  "Prompts for a project to open in a new tab. The project opens
immediately to the project's magit status buffer (or equivalent
for other VCS). The tab is named after the project name."
  (interactive)
  (ivy-read (projectile-prepend-project-name "Open project in new tab: ")
            projectile-known-projects
            :preselect (and (projectile-project-p)
                            (abbreviate-file-name (projectile-project-root)))
            :action 'dr/open-project-in-new-tab-action
            :require-match t
            :sort counsel-projectile-sort-projects
            :caller 'dr/open-project-in-new-tab))
