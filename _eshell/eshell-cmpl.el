;; * switch-to-buffer
(defun eshell/switch-to-buffer(buf)
  (switch-to-buffer buf))
(defun pcomplete/eshell-mode/switch-to-buffer()
  (if (string-match "^switch-to-buffer\\ \\{2,\\}$" (eshell-get-old-input))
      (insert-string "\\*")
    (progn
      (pcomplete-here
       (reverse (sort (mapcar 'buffer-name 
        (buffer-list)) 'string<))))))




;; *
(provide 'eshell-cmpl)
