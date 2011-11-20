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
