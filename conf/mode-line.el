;; Override the `mode-line' face, making it taller.
(set-face-attribute 'mode-line nil
                    :background "#394952"
                    :box '(:line-width 8 :color "#394952"))

;; Override the `mode-line-inactive' face. It has a darker colour than the
;; active face, and it is not quite as tall.
(set-face-attribute 'mode-line-inactive nil
                    :background "#2e363b"
                    :box '(:line-width 5 :color "#2e363b"))

(defface dr/mode-line-file-name-face
  '((t
     :weight bold
     :foreground "SpringGreen3"
     :background nil
     :box nil))
  "Face for the file's name (or the buffer name if the buffer
isn't visiting a file)."
  :group 'dr/faces)

(defface dr/mode-line-file-name-modified-face
  '((t
     :weight bold
     :foreground "firebrick1"
     :background nil
     :box nil))
  "Face for the file's name if the file is modified."
  :group 'dr/faces)

(defface dr/mode-line-project-face
  '((t
     :weight normal
     :foreground "#8db6cd" ;; THis colour is "SkyBlue3".
     :background nil
     :box nil))
  "Face for the file's project."
  :group 'dr/faces)

(defface dr/mode-line-file-dir-face
  '((t
     :weight normal
     :background nil
     :box nil))
  "Face for the file's directory."
  :group 'dr/faces)

(defface dr/mode-line-vc-branch-face
  '((t
     :weight normal
     :slant italic
     :foreground "#9f79ee" ;; This colour is "MediumPurple2".
     :background nil
     :box nil))
  "Face for the version control branch."
  :group 'dr/faces)

(defface dr/mode-line-cursor-position-face
  '((t
     :weight normal
     :foreground "#8b7b8b" ;; This colour is "thistle4".
     :background nil
     :box nil))
  "Face for the line/column info."
  :group 'dr/faces)

;; An `alist' cache of values that are (relatively) expensive to compute on
;; every mode line redraw.
(setq *dr/mode-line-cache* nil)

(defun dr/mode-line-populate-cache ()
  "Populate all of the values (buffer local) we keep in the cache
for each mode line redraw."
  (setq-local *dr/mode-line-cache*
              `((project-name . ,(projectile-project-name))
                (project-root . ,(projectile-project-root)))))

(defun dr/mode-line-get (key)
  "Get a value from the mode line values cache. If the cache
isn't yet populated, populates it first."
  (if *dr/mode-line-cache*
      nil
      (dr/mode-line-populate-cache))
  (cdr (assoc key *dr/mode-line-cache*)))

(defun dr/mode-line-file-name-or-buffer-name ()
  "For use in `mode-line-format'. Returns the file name if the
current buffer is visiting a file, otherwise returns the '%b'
construct (which will print the buffer name as-is)."
  (let ((name (buffer-file-name)))
    (if name
        (file-name-nondirectory name)
      "%b")))

(defun dr/mode-line-project ()
  "For use in `mode-line-format'. Returns the projectile project
name (from cache) if the buffer is within the context of a
project. Otherwise, an empty string is returned."
  (let ((project (dr/mode-line-get 'project-name)))
    (if project (format "[%s] " project) "")))

(defun dr/mode-line-file-dir ()
  "For use in `mode-line-format'. If the buffer is visiting a
  file and the file is part of a projectile project, returns the
  directory path relative to the project root (best used in
  conjunction with `dr/mode-line-project'). If the buffer is
  visiting a file and the the file is not part of a projectile
  project, the directory path is returned. Returns an empty
  string if the the file is not visiting a file."
  (let ((name (buffer-file-name))
        (project (dr/mode-line-get 'project-name)))
    (if name
        (let ((dir (file-name-directory name)))
          (if project
              (file-relative-name dir (dr/mode-line-get 'project-root))
            dir))
      "")))

;; Set the (default) template for the mode line.
(setq-default mode-line-format
              (list
	       mode-line-front-space

               ;; The default emacs mode line includes the following, but they
               ;; are omitted here:
               ;;
               ;;   `mode-line-mule-info'
               ;;   `mode-line-client'
               ;;   `mode-line-remote'
               ;;
               ;; We also omit the %p and %I constructs, which are the position
               ;; in the buffer (the percent above the top window), and the
               ;; overall size of the buffer, respectively.

	       mode-line-modified

	       " "

               ;; The file (or buffer) name. The face is selected based on
               ;; whether the buffer is modified or not.
               '(:eval
                 (let ((name (dr/mode-line-file-name-or-buffer-name))
	               (face
                        (if (buffer-modified-p)
                            'dr/mode-line-file-name-modified-face
                          'dr/mode-line-file-name-face)))
	           (propertize name
                               'face face
                               'help-echo (buffer-file-name))))

               " "

               ;; The buffer's project and/or file's directory (which may be
               ;; relative to the project root if we're in a project).
               '(:eval
                 (propertize (dr/mode-line-project)
                             'face 'dr/mode-line-project-face))
               '(:eval
                 (propertize (dr/mode-line-file-dir)
                             'face 'dr/mode-line-file-dir-face))

               " "
               ;; The line and column counts (%c starts the column count at 0,
               ;; and %C at 1).
               (propertize "-%l '%C" 'face 'dr/mode-line-cursor-position-face)
               " "
               ;; The version control branch.
               '(:eval
                 (propertize (if vc-mode (substring vc-mode 5) "")
                             'face 'dr/mode-line-vc-branch-face
                             'help-echo nil))


               ;; Add spaces to right align the major mode.
               '(:eval
                 (propertize " "
                             'display `((space
                                         :align-to
                                         (- (+ right right-fringe right-margin)
                                            ,(+ (+ 3 (string-width mode-name))))))))

               ;; The major mode.
               (propertize " %m "
                           'face 'font-lock-string-face
                           'help-echo nil)
               ))
