;; -*- encoding: utf-8-unix; -*-
;; File-name:    <del-spc-eol.el>
;; Create:       <2011-11-11 21:44:37 ran9er>
;; Time-stamp:   <2011-11-11 21:45:23 ran9er>
;; Mail:         <2999am@gmail.com>

;; * del-spc-eol
(defun del-spc-eol
  (save-excursion
    (replace-regexp "[ \t]+$" "" nil 1 (point-max))))

