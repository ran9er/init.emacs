;; -*- encoding: utf-8-unix; -*-
;; File-name:    <hash.el>
;; Create:       <2011-12-27 21:01:55 ran9er>
;; Time-stamp:   <2011-12-27 21:02:18 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun cons2hash (lst)
  (let ((h (make-hash-table :test 'equal)))
    (mapc (lambda(x)(puthash (car x)(cdr x) h)) lst)
    h))

;;;###autoload
(defun build-hash-table (&rest lst)
  (cons2hash (cons-list-l lst)))

;;;###autoload
(defun list-hash (hash-table &optional with-value)
  (let (lst)
    (if with-value
        (maphash (lambda(x y)(setq lst (cons (cons x y) lst))) hash-table)
      (maphash (lambda(x y)(setq lst (cons x lst))) hash-table))
    (reverse lst)))
