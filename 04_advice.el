(defadvice isearch-yank-word-or-char (around aiywoc activate)
  ;; default-key: isearch-mode-map C-w
  (interactive)
  (isearch-yank-string
   (if mark-active
       (buffer-substring-no-properties
        (region-beginning) (region-end))
     (current-word nil nil)))
  (deactivate-mark))

(defadvice comment-or-uncomment-region (before slickcomment activate compile)
  "When called interactively with no active region, toggle comment on current line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice what-cursor-position (around what-cursor-position-around activate)
  "When called interactively with active region, print info of region instead."
  (if mark-active
      (let ((beg (region-beginning))
            (end (region-end)))
        (message "Region: begin=%d end=%d length=%d"
                 beg end (- end beg)))
    ad-do-it))

(defadvice delete-horizontal-space (around resize-space (&optional backward-only))
  "if elop or bolp or space around \"(\" or \")\", delete all space;"
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
        ad-do-it
      ad-do-it
      (insert " ")
      (if bwd-p (backward-char 1)))))

(defadvice kill-line (around merge-line (&optional arg) activate)
  "if this line is not empty and cursor in the end of line, merge next N line"
  (interactive "P")
  (let ((n (or arg 1)))
    (if (and (null (bolp)) (eolp))
        (while (< 0 n)
          (delete-char 1)
          (delete-horizontal-space)
          (if (< 1 n) (end-of-line))
          (setq n (1- n)))
      ad-do-it)))

(defadvice kill-ring-save (around slick-copy activate)
  "When called interactively with no active region, copy a single line instead."
  (if (or (use-region-p) (not (called-interactively-p 'interactive)))
      ad-do-it
    (kill-new (buffer-substring (line-beginning-position)
                                (line-beginning-position 2))
              '(yank-line))
    (message "Copied line")))

(defadvice kill-region (around slick-copy)
  "When called interactively with no active region, kill a single line instead."
  (if (or (use-region-p) (not (called-interactively-p 'interactive)))
      ad-do-it
    (kill-new (filter-buffer-substring (line-beginning-position)
                                       (line-beginning-position 2) t)
              '(yank-line))))

(defadvice kill-region (before smart-kill)
  (let ((p (point))
        (i (save-excursion (abs (skip-chars-backward " \t")))))
    (setq end p)
    (cond
     ((if mark-active
          (setq beg (mark))))
     ((< 0 i)
       (if (zerop (mod i tab-width))
           (setq beg (- p tab-width))
         (setq beg (- p (mod i tab-width)))))
     (t
      (progn (backward-word)
             (setq beg (point)))))))
;; (ad-activate 'kill-region)
;; (ad-deactivate 'kill-region)

(defadvice kill-rectangle (after copy-to-kill-ring activate)
  (interactive "r\nP")
  (kill-new (mapconcat 'identity killed-rectangle "\n")))

(defun yank-line (string)
  "Insert STRING above the current line."
  (beginning-of-line)
  (unless (= (elt string (1- (length string))) ?\n)
    (save-excursion (insert "\n")))
  (insert string))

(defadvice move-beginning-of-line (around nai-dhs activate compile)
  "When remap newline to newline-and-indent, use this"
  (if (eq last-command 'newline-and-indent)
      (delete-horizontal-space)
    ad-do-it))
