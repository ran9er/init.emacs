;; -*- encoding: utf-8-unix; -*-
;; File-name:    <outside-macro.el>
;; Create:       <2011-12-28 21:54:14 ran9er>
;; Time-stamp:   <2011-12-28 21:55:56 ran9er>
;; Mail:         <2999am@gmail.com>

;; outside
(defmacro outside (o b s)
  "up list N level, append PRE ahead and SUF behind, backward M char"
  `(lambda(&optional n)
     (interactive "P")
     (let ((x (if n (prefix-numeric-value n) 1))
           beg end tmp)
       (if mark-active
           (setq beg (region-beginning)
                 end (region-end))
         (up-list x)
         (setq end (point))
         (setq beg (backward-list))
         (while (member (char-to-string (get-byte (1- beg)))
                        '("'" "`" "," "#" "@"))
           (setq beg (1- beg))))
       (setq tmp (buffer-substring-no-properties beg end))
       (delete-region beg end)
       (insert ,o)
       (backward-char ,b)
       (save-excursion
         (insert ,s tmp)))))
;(def-key-s 0 "C-9" (outside "()" 1 " "))
