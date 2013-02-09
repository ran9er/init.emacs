;; outside
;;;###autoload
(defun outside (o b s &optional n)
  "up list N level, append PRE ahead and SUF behind, backward M char"
  (interactive "P")
  (let ((x (if n (prefix-numeric-value n) 1))
        beg end tmp delimiter)
    (if mark-active
        (setq delimiter ""
              beg (region-beginning)
              end (region-end))
      (setq delimiter s)
      (up-list x)
      ;; (setq end (point))
      ;; (setq beg (backward-list))
      ;; (while (member (char-to-string (get-byte (1- beg)))
      ;;                '("'" "`" "," "#" "@"))
      ;;   (setq beg (1- beg)))
      (setq end (point)
            beg (+ (backward-list)(skip-chars-backward  "'`,#@"))))
    (setq tmp (buffer-substring-no-properties beg end))
    (delete-region beg end)
    (insert o)
    (backward-char b)
    (save-excursion
      (insert delimiter tmp))))

;;;###autoload
(defun outside-kill (&optional n)
  "up list N level, append PRE ahead and SUF behind, backward M char"
  (interactive "P")
  (let ((x (if n (prefix-numeric-value n) 1))
        beg end tmp)
    (if mark-active
        (setq beg (region-beginning)
              end (region-end))
      (up-list 1)
      (setq end (point)
            beg (+ (backward-list)(skip-chars-backward  "'`,#@"))))
    (setq tmp (buffer-substring-no-properties beg end))
    (delete-region beg end)
    (up-list x)
    (delete-region
     (point)
     (+ (backward-list)(skip-chars-backward  "'`,#@")))
    (save-excursion
      (insert tmp))))
