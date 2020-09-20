(defun jump-15-lines-forwards ()
  "Jumps 15 lines forwards in the buffer. A small convenience
instead of running C-u 15 C-n. Only useful if bound to, for
example, M-n."
  (interactive)
  (next-line 15))

(defun jump-15-lines-backwards ()
  "Jumps 15 lines backwards in the buffer. A small convenience
instead of running C-u 15 C-p. Only useful if bound to, for
example, M-p."
  (interactive)
  (previous-line 15))

(defun goto-next-quote ()
  "Moves the cursor to the next quotation mark or backtick."
  (interactive)
  (forward-char)
  (search-forward-regexp "[\"\|'|`]")
  (backward-char))

(defun goto-previous-quote ()
  "Moves the cursor to the previous quotation mark or backtick."
  (interactive)
  (search-backward-regexp "[\"\|']|`"))

(defun rename-current-file (new-name)
  "Rename the file of the current buffer, and automatically
replace the buffer with the new file. Scroll position is
preserved."
  (interactive "sRename buffer to: ")
  (let ((p (window-start nil)))
    (rename-file (buffer-file-name (current-buffer)) new-name)
    (find-alternate-file new-name)
    (set-window-start nil p)))

(defun copy-and-comment-region (beg end &optional arg)
  "Duplicate and comment out the selected line(s).
See `comment-region' for behavior of a prefix arg."
  (interactive "r\nP")
  (if (not (region-active-p))
      (setq beg (point)
            end (point)))
  (save-excursion
    (setq beg (progn (goto-char beg) (line-beginning-position))
          end (progn (goto-char end) (line-end-position)))
    (copy-region-as-kill beg end)
    (goto-char end)
    (yank)
    (comment-region beg end arg))
  (deactivate-mark)
  (next-line)
  (back-to-indentation))

(defun csv-highlight (&optional separator)
  "Colour-code CSV columns.

Courtesy of:
  https://www.reddit.com/r/emacs/comments/26c71k/csv_column_highlighting/"
  (interactive (list (when current-prefix-arg (read-char "Separator: "))))
  (font-lock-mode 1)
  (let* ((separator (or separator ?\,))
         (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
         (colors (loop for i from 0 to 1.0 by (/ 2.0 n)
                       collect (apply #'color-rgb-to-hex
                                      (color-hsl-to-rgb i 0.3 0.5)))))
    (loop for i from 2 to n by 2
          for c in colors
          for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
          do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))

(defun enter-visual-line-mode ()
  "Helper for initializing visual-line-mode."
  (interactive)
  (toggle-truncate-lines -1)
  (auto-fill-mode -1)
  (turn-on-visual-line-mode))
