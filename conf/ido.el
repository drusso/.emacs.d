;; Set up ido (with flx).
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)

(setq ido-enable-flex-matching t
      flx-ido-threshold 1000 ;; The default is 6000.
      ido-max-prospects 15

      ;; Some options to try out:
      ;;
      ;; ido-enable-prefix nil
      ;; ido-auto-merge-work-directories-length nil
      ;; ido-create-new-buffer 'always
      ;; ido-use-filename-at-point 'guess
      ;; ido-use-virtual-buffers t
      ;; ido-handle-duplicate-virtual-buffers 2
      )

;; Display ido results vertically, rather than horizontally.
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(define-key ido-completion-map (kbd "C-n") 'ido-next-match)
(define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
