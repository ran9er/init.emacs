(defun copy-word-on-column (&optional n strict really-word)
  (interactive "P")
  (save-excursion
    (let ((c (current-column))
          (n (if mark-active
                 (prog1
                     (count-lines (region-beginning)(region-end))
                   (goto-char (region-beginning)))
               (or n 1)))
          lst)
      (while (> n 0)
        (setq n (1- n))
        (move-to-column c)
        (setq lst (cons (current-word strict really-word) lst))
        (forward-line))
      (kill-new (mapconcat 'identity (reverse lst) "\n")))))
