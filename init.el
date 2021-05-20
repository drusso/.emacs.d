;; -*- mode: emacs-lisp -*-

(defun load-conf (conf-file-name)
  (load (concat user-emacs-directory
                (file-name-as-directory "conf")
                conf-file-name)))

(load-conf "custom.el")
(load-conf "bindings.el")
(load-conf "frames.el")
(load-conf "windows.el")
(load-conf "packages.el")
(load-conf "mouse.el")
(load-conf "tabs.el")
(load-conf "term.el")
(load-conf "visual.el")
(load-conf "ivy.el")
(load-conf "projectile.el")
(load-conf "mode-line.el")
(load-conf "whitespace.el")
(load-conf "misc.el")
(load-conf "modes/compilation-mode.el")
(load-conf "modes/diff-mode.el")
(load-conf "modes/git-rebase-mode.el")
(load-conf "modes/grep-mode.el")
(load-conf "modes/idle-highlight-mode.el")
(load-conf "modes/magit-mode.el")
(load-conf "modes/org-mode.el")
(load-conf "modes/rust-mode.el")
(load-conf "modes/text-mode.el")

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      inhibit-startup-screen t
      ring-bell-function 'ignore
      smex-save-file (concat user-emacs-directory ".smex-items")
      visible-bell nil
      enable-recursive-minibuffers t
      )

;; Don't uniquify buffer names, instead, always use the entire file path. Note
;; that there are customizations in mode-line.el to accomodate this style of
;; buffer naming.
(setq uniquify-buffer-name-style 'forward
      uniquify-min-dir-content 999
      )

(setq-default fill-column 79
	      imenu-auto-rescan t
	      truncate-lines t
	      )

(prefer-coding-system 'utf-8)

;; Accept `y` or `n` immediately in a prompt.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable `dired-find-alternate-file`. If enabled, pressing the key `a` will
;; replace the existing dired buffer instead of starting a new buffer.
(put 'dired-find-alternate-file 'disabled nil)

(smex-initialize)
(save-place-mode 1)

(load-conf "hack-load-history.el")

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Set tramp's verbosity (0 = silent, 1 = error, 2 = warning, etc.)
(setq tramp-verbose 1)

;; Have save-desktop-mode remember all files, including remote SSH files.
(setq desktop-files-not-to-save "^$")

;; Disable vc's other version control backends.
(setq vc-handled-backends '(Git))

(server-start)
