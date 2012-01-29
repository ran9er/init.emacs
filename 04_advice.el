;; -*- encoding: utf-8-unix; -*-
;; File-name:    <04_advice.el>
;; Create:       <2012-01-16 13:44:23 ran9er>
;; Time-stamp:   <2012-01-29 11:05:33 ran9er>
;; Mail:         <2999am@gmail.com>

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

(defadvice delete-horizontal-space (around resize-space (&optional backward-only) activate)
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

;; (defadvice kill-region (before smart-kill (beg end) activate)
;;   (let ((p (point))
;;         (i (save-excursion (abs (skip-chars-backward " \t")))))
;;     (cond
;;      ((if mark-active
;;           (setq beg (mark)
;;                 end p)))
;;      ((< 0 i)
;;        (if (zerop (mod i tab-width))
;;            (setq beg (- p tab-width)
;;                  end p)
;;          (setq beg (- p (mod i tab-width))
;;                end p)))
;;      (t
;;       (progn (backward-word)(setq beg (point)
;;                                   end p))))))

