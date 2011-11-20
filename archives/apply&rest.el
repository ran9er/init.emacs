(defun add-to-list-x (LIST-VAR &rest REST)
 (add-to-list-l LIST-VAR REST))

(defun add-to-list-l (LIST-VAR LIST)
  (apply 'add-to-list-x LIST-VAR LIST))
