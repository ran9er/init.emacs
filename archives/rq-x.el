;;  rq-x
(defun rq-x (action lst)
  "(rq-x 'require
        '(aaa bbb ccc ...))"
  (let ((action (cond ((eq action 0) 'require)(t action))))
    (mapcar (lambda(ext) (funcall action ext)) lst)))

(defmacro rqx (action &rest lst)
  "(rqx 0 aaa bbb ccc)"
  ;; (list 'rq-x `',action `',lst))
  `(rq-x ',action ',lst))

;; 2012-01-16 <1> 22:39:41
(defun rq-x (action &rest rest)
    (while (car rest)
      (let ((def (pop rest)))
        (funcall action def)
        )))

(rq-x 'require
          'cedet
          'eieio
          'semantic
          'srecode
          'ede
          'speedbar
)
