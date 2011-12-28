;; -*- encoding: utf-8-unix; -*-
;; File-name:    <string.el>
;; Create:       <2011-12-27 21:26:39 ran9er>
;; Time-stamp:   <2011-12-27 21:53:15 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun join-string (lst s)
 "(mapconcat 'concat lst s)"
  (if (caddr lst)
      (concat (car lst) s (join-string (cdr lst) s))
    (concat (car lst) s (cadr lst))))

;;;###autoload
(defmacro shell-command-symbol-to-string (&rest s)
  `(shell-command-to-string
    (apply 'concat (mapcar
     (lambda(x)(concat (symbol-name x) " "))
     ',s))))

;;;###autoload
(defalias 'ss 'shell-command-symbol-to-string)
