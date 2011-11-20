;; -*- encoding: utf-8-unix; -*-
;; File-name:    <outside.el>
;; Create:       <2011-11-10 15:27:06 ran9er>
;; Time-stamp:   <2011-11-10 15:27:09 ran9er>
;; Mail:         <2999am@gmail.com>
(defun outside (str bk &optional n)
  "up list N level, append STR , backward BK char"
  (let ((x (if n (prefix-numeric-value n) 1))
        p q)
    (up-list x)
    (setq p (point))
    (backward-list)
    (setq q (point))
    (while (member (char-to-string (get-byte (1- q))) 
                   '("'" "`" "," "#"))
      (setq q (1- q)))
    (kill-region q p)
    (insert-string str)
    (backward-char bk)
    (save-excursion
      (insert-string " ")
      (yank))))
(defun outside-list (&optional n)
  "See also `outside'"
  (interactive "P")
  (outside "()" 1 n))
