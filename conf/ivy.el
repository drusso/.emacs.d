(require 'swiper)

(ivy-mode)
(counsel-mode)

(setq ivy-height 15)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-use-virtual-buffers t)
(setq ivy-use-selectable-prompt t)
(setq ivy-re-builders-alist
      ;; Completion style choices are:
      ;;   ivy--regex
      ;;   ivy--regex-plus
      ;;   ivy--regex-ignore-order
      ;;   ivy--regex-fuzzy
      ;;   regexp-quote
      '((t . ivy--regex-plus))
      )
(setq ivy-display-style 'fancy)
(setq swiper-include-line-number-in-search t)

;; Setup for swiper integraton with mc (multiple cursors).
(setq mc/cmds-to-run-once '(swiper-mc))

(defun dr/swiper-C-r (&optional arg)
  "Move cursor vertically up ARG candidates. If the input is
empty, select the previous history element instead.

Like swiper's `swiper-C-s', but in the reverse
direction. Intended to emulate `isearch-backward'."
  (interactive "p")
  (if (string= ivy-text "")
      (ivy-previous-history-element 1)
    (ivy-previous-line arg)))

;; Emulate isearch-backward in the swiper minibuffer.
(define-key swiper-map (kbd "C-r") 'dr/swiper-C-r)

;; Support C-s/C-r in the ivy minibuffer.
(define-key ivy-minibuffer-map (kbd "C-s") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line)

;; When the selected candidate is a directory, continue completion with that
;; directory, rather than open dired.
(define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
