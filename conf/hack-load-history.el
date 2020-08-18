;;; hack-load-history.el --- a workaround for a bug resulting from a poisoned
;;;                          `load-history' alist

;;; Commentary:

;; This is a workaround for an issue preventing some packages/modes -- notably
;; magit -- from loading due to an unhandled error.

;; The error is caused by an invalid value(s) that poisons the `load-history'
;; alist. The source of or reason for the invalid value is unknown.

;; When, for example, magit is started, a call is made to
;; `load-history-filename-element'[1], which reads an invalid value from
;; `load-history', which then results in an error when attempting to use the
;; unexpected value.

;; The issue was reported to affect Android in 2016, but now seems to affect
;; macOS + Emacs 27.1 (released in 2020). Refer to [2] for further details.

;; Refer to [3] for an alternate approach to a fix.

;;; References:

;;  [1] https://github.com/emacs-mirror/emacs/blob/emacs-27.1/lisp/subr.el#L4504
;;  [2] https://github.com/termux/termux-packages/issues/423
;;  [3] https://gist.github.com/Mihara/021329452c8aaddca657cee46a84f3e3

;;; Code:

(defun dr/make-safe (fn-symbol)
  "Advises around a function to catch and log any errors from
that function. Returns nil on any error."
  (let ((error-format (format "Warn: Ignoring error from %s: %%S" fn-symbol)))
    (advice-add fn-symbol
                :around
                `(lambda (fn &rest args)
                   (with-demoted-errors ,error-format (apply fn args))))))

(if (and (equal system-type 'darwin)
         (equal emacs-version "27.1"))
    (progn
      (dr/make-safe 'load-history-filename-element)
      (message "Applied workaround to ignore errors from load-history-filename-element"))
    (message "Not applying workaround to ignore errors from load-history-filename-element"))
