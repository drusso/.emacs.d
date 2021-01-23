;; Make a global variant of idle-highlight-mode so we can turn it on by default
;; in all buffers.
(define-globalized-minor-mode global-idle-highlight-mode idle-highlight-mode
  (lambda () (idle-highlight-mode 1)))

;; Turn it on globally by default.
(global-idle-highlight-mode 1)

;; Override the idle-highlight face so the highlight is just an underline.
(set-face-attribute
 'idle-highlight nil
  :inherit nil
  :underline '(:color "yellow2" :style line))
