;; See misc.el for the definition of enter-visual-line-mode.
(add-hook 'org-mode-hook 'enter-visual-line-mode)

;; Use org-indent-mode by default.
(add-hook 'org-mode-hook (lambda () (org-indent-mode t)) t)
