;; -*- encoding: utf-8-unix; -*-
;; File-name:    <+arith.el>
;; Create:       <2012-04-10 23:05:51 ran9er>
;; Time-stamp:   <2012-04-10 23:23:21 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun average (&rest lst)
  (apply (lambda(&rest x)(/ (float (apply '+ x)) (length x)))
         lst))
