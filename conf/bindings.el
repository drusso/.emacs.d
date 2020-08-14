;; Set the right command key as control. This works in Cocoa Emacs, for the
;; most part, except macOS handles Cmd-Space (and related keybindings) before
;; Emacs can.
(setq mac-right-command-modifier 'control)

;; Emacs is blind to the Command-Space (i.e. Cmd-Space) key press, so we have
;; to bind it to something else.
(global-set-key (kbd "C-<return>") 'set-mark-command)

;; Smex (a "M-x" enhancement)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Buffer ovement
(global-set-key (kbd "C-,") 'backward-paragraph)
(global-set-key (kbd "C-.") 'forward-paragraph)
(global-set-key (kbd "M-p") 'jump-15-lines-backwards)
(global-set-key (kbd "M-n") 'jump-15-lines-forwards)
(global-set-key (kbd "M-'") 'goto-next-quote)
(global-set-key (kbd "M-;") 'goto-previous-quote)

;; Regions
(global-set-key (kbd "C-;") 'er/mark-inside-quotes)
(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C-/") 'exchange-point-and-mark)

;; Commenting
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-C") 'copy-and-comment-region)

;; Window navigation
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<left>") 'windmove-left)
(global-set-key (kbd "s-<up>") 'windmove-up)
(global-set-key (kbd "s-<down>") 'windmove-down)

;; Search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-S-s") 'flx-isearch-forward)
(global-set-key (kbd "C-S-r") 'flx-isearch-backward)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Multiple cursors
(global-set-key (kbd "s-[") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "s-]") 'mc/edit-ends-of-lines)
(global-set-key (kbd "s-\\") 'mc/edit-lines)
(global-set-key (kbd "s-{") 'mc/mark-previous-like-this)
(global-set-key (kbd "s-}") 'mc/mark-next-like-this)
(global-set-key (kbd "s-|") 'mc/mark-all-in-region)
(global-set-key (kbd "s-<mouse-1>") 'mc/add-cursor-on-click)

;; Projectile & buffers
(global-set-key (kbd "C-`") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-~") 'switch-to-buffer)
(global-set-key (kbd "C--") 'switch-to-prev-buffer)
(global-set-key (kbd "C-=") 'switch-to-next-buffer)
(global-set-key (kbd "C-0") 'kill-this-buffer)
(global-set-key (kbd "C-)") 'projectile-kill-buffers)
(global-set-key (kbd "M-`") 'projectile-switch-open-project)
(global-set-key (kbd "M-<tab>") 'projectile-switch-project)
(global-set-key [(super t)] 'projectile-find-file)
(global-set-key [(super r)] 'projectile-grep)

;; Miscellaneous
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c q") 'join-line)
(global-set-key (kbd "M-Q") 'fill-region)
(global-set-key (kbd "s-.") 'imenu)
