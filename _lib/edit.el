;;;###autoload
(defun resize-horizontal-space (&optional backward-only)
  (interactive "*P")
  (let ((orig-pos (point))
        (skip-chars " \t")
        (delimit-char
         (mapcar (lambda (x) (string-to-char x))
                 '("(" ")")))
        fwd-pos fwd-p bwd-pos bwd-p)
    (setq
     fwd-pos (progn (skip-chars-forward skip-chars)(eolp))
     fwd-p  (memq (following-char) delimit-char)
     bwd-pos (progn (skip-chars-backward skip-chars)(bolp))
     bwd-p  (memq (preceding-char) delimit-char))
    (goto-char orig-pos)
    (if (or fwd-pos bwd-pos (and fwd-p bwd-p))
        (delete-horizontal-space backward-only)
      (delete-horizontal-space backward-only)
      (insert " ")
      (if bwd-p (backward-char 1)))))

;;;###autoload
(defun smart-backward-kill ()
  (interactive)
  (let ((i (save-excursion (abs (skip-chars-backward " \t")))))
    (cond
     (mark-active
      (call-interactively 'kill-region))
     ((< 0 i)
      (backward-delete-char
       (if (zerop (mod i tab-width)) tab-width (mod i tab-width))))
     (t
      (call-interactively 'backward-kill-word)))))

;;;###autoload
(defun string-expand (s)
  (interactive "s")
  (let  ((rs (buffer-substring-no-properties (region-beginning) (region-end))))
    (call-interactively 'kill-region)
    (insert (format (replace-regexp-in-string "|" "%s" s) rs))))
