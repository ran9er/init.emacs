;;;###autoload
(defmacro xor (fn &rest lst)
  "like find-if"
  (let ((lst (arg-parse lst)))
    `(or ,@(mapcar (lambda(x)(and (funcall fn x) x)) lst))))

;;;###autoload
(defmacro xand (fn &rest lst)
  (let ((lst (arg-parse lst)))
    `(and ,@(mapcar (lambda(x)(and (funcall fn x) x)) lst))))
