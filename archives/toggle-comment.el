;; -*- encoding: utf-8-unix; -*-
;; File-name:    <toggle-comment.el>
;; Create:       <2011-12-01 09:34:30 ran9er>
;; Time-stamp:   <2011-12-01 09:34:57 ran9er>
;; Mail:         <2999am@gmail.com>
;; * toggle-comment-region
(defun toggle-comment-region (beg end &optional n)
  "Comment the lines in the region if the first non-blank line is
commented, and conversely, uncomment region. If optional prefix arg
N is non-nil, then for N positive, add N comment delimiters or for N
negative, remove N comment delimiters.
Uses `comment-region' which does not place comment delimiters on
blank lines."
  (interactive "r\nP")
  (if n
      (comment-region beg end (prefix-numeric-value n))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      ;; skip blank lines
      (skip-chars-forward " \t\n")
      (if (looking-at (concat "[ \t]*\\(" (regexp-quote comment-start) "+\\)"))
          (uncomment-region beg end)
        (comment-region beg end)))))
