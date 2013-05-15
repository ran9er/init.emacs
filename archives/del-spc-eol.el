;; -*- encoding: utf-8-unix; -*-
;; File-name:    <del-spc-eol.el>
;; Create:       <2011-11-11 21:44:37 ran9er>
;; Time-stamp:   <2011-11-20 10:14:48 ran9er>
;; Mail:         <2999am@gmail.com>

;; * del-spc-eol
(defun del-spc-eol ()
  (interactive)
  (save-excursion
    (replace-regexp "[ \t]+$" "" nil (point-min) (point-max))))

