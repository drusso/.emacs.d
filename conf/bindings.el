;; Set the right command key as control. This works in Cocoa Emacs, for the
;; most part, except macOS handles Cmd-Space (and related keybindings) before
;; Emacs can.
(setq mac-right-command-modifier 'control)

;; Emacs is blind to the Command-Space (i.e. Cmd-Space) key press, so we have
;; to bind it to something else.
(global-set-key (kbd "C-<return>") 'set-mark-command)

;; A "M-x" enhancement. Note `counsel-M-x' is enhanced by smex when installed.
(global-set-key (kbd "M-x") 'counsel-M-x)

;; Buffer movement
(global-set-key (kbd "C-,") 'backward-paragraph)
(global-set-key (kbd "C-.") 'forward-paragraph)
(global-set-key (kbd "M-p") 'dr/jump-15-lines-backwards)
(global-set-key (kbd "M-n") 'dr/jump-15-lines-forwards)

;; Regions
(global-set-key (kbd "C-;") 'er/mark-inside-quotes)
(global-set-key (kbd "C-'") 'er/expand-region)

;; Commenting
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-C") 'dr/copy-and-comment-region)

;; Window navigation
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<left>") 'windmove-left)
(global-set-key (kbd "s-<up>") 'windmove-up)
(global-set-key (kbd "s-<down>") 'windmove-down)

;; Tab navigation
(global-set-key (kbd "s-S-<right>") 'tab-next)
(global-set-key (kbd "s-S-<left>") 'tab-previous)
(global-set-key (kbd "s--") 'tab-bar-select-tab-by-name)
(global-unset-key (kbd "s-_"))
(global-set-key (kbd "s-=") 'dr/new-tab)
(global-set-key (kbd "s-+") 'dr/open-project-in-new-tab)
(dotimes (i 9) ;; Bind s-1 through s-9 to tabs.
  (let ((d (+ i 1)))
    (global-set-key (kbd (format "s-%d" d))
                    `(lambda ()
                       (interactive)
                       (tab-bar-select-tab ,d)))))
(global-set-key (kbd "s-0") 'tab-close)

;; Search
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch)
(global-set-key (kbd "C-S-s") 'swiper-isearch-thing-at-point)
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
(global-set-key (kbd "C-`") 'counsel-projectile-switch-to-buffer)
(global-set-key (kbd "C-~") 'counsel-switch-buffer)
(global-set-key (kbd "C--") 'switch-to-prev-buffer)
(global-set-key (kbd "C-=") 'switch-to-next-buffer)
(global-set-key (kbd "C-0") 'kill-this-buffer)
(global-set-key (kbd "C-)") 'projectile-kill-buffers)
(global-set-key (kbd "M-`") 'dr/counsel-projectile-switch-open-project)
(global-set-key (kbd "M-<tab>") 'counsel-projectile-switch-project)
(global-set-key [(super t)] 'counsel-projectile-find-file)
(global-set-key [(super r)] 'counsel-projectile-grep)

;; Miscellaneous
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c q") 'join-line)
(global-set-key (kbd "M-Q") 'fill-region)
(global-set-key (kbd "s-.") 'imenu)
