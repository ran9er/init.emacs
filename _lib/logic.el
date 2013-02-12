;; -*- encoding: utf-8-unix; -*-
;; File-name:    <+logic.el>
;; Create:       <2012-02-17 22:20:09 ran9er>
;; Time-stamp:   <2012-02-17 22:20:28 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defmacro xor (fn &rest lst)
  "like find-if"
  (let ((lst (arg-parse lst)))
    `(or ,@(mapcar (lambda(x)(and (funcall fn x) x)) lst))))

;;;###autoload
(defmacro xand (fn &rest lst)
  (let ((lst (arg-parse lst)))
    `(and ,@(mapcar (lambda(x)(and (funcall fn x) x)) lst))))
