;; Note text-mode is the parent mode of:

;;   markdown-mode
;;   html-mode (parent is mhtml-mode)
;;   mhtml-mode
;;   sqml-mode
;;

;; See misc.el for the definition of dr/enter-visual-line-mode.
(add-hook 'text-mode-hook 'dr/enter-visual-line-mode)
