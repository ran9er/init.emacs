(defun mask-mk-lst (x)
  (let ((l (lambda(i n r)
             (if (< i n)
                 (funcall l (1+ i) n (cons (* (car r) 2) r))
               r))))
    (funcall l 1 x '(1))))

;;;###autoload
(defun mask-gen-num(&rest lst)
  (let ((r 1))
    (apply
     '+
     (mapcar
      (lambda(x)
        (prog1 (if x r 0)
          (setq r (* 2 r))))
      lst))))

;;;###autoload
(defun mask-check (idx n)
  (null (zerop (logand (expt 2 (1- idx)) n))))

;;;###autoload
(defun mask-gen-lst (n)
  (let ((i (1+ (logb n))) r)
    (while (> i 0)
      (setq r (cons (mask-check i n) r))
      (setq i (1- i)))
    r))

;;;###autoload
(defun mask-count-t (n)
  (length (remove nil (mask-gen-lst n))))

