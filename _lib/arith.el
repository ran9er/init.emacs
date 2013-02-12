;;;###autoload
(defun average (&rest lst)
  (/ (float (apply '+ lst)) (length lst)))
