;; (define-key liny-keymap "\C-l"
;;   (lambda()(interactive)
;;     (let* ((ov (liny-get-overlay))
;;           (f (overlay-get ov 'liny-modifi-callback)))
;;       (funcall f ov t))))

(setq liny-syntax-delimiter
      (cons
       (cons "\\(\\$\\)(" 'liny-eval)
       (remove
        (assoc "\\(\\$\\)(" liny-syntax-delimiter)
        liny-syntax-delimiter)))

(defun liny-eval (s &optional p o)
  (let ((f (read s)))
    (if (and (listp f)(null (functionp f)))
        (eval
         `(let ((str ,s)(pos ,p)(ovl ,o)) ,f))
      (overlay-put o 'liny-modifi-callback f)
      (liny-primary-append-hooks o 'liny-call))))

(defun liny-call (ov after-p beg end &optional length)
  (let* ((prim (liny-get-primary ov))
         (pf (overlay-get prim 'liny-modifi-callback)))
    (if (overlay-get prim 'ready)
        (let ((mirrors (overlay-get prim 'mirrors)))
          (if mirrors
              (mapc
               (lambda(mir)
                 (let ((mf (overlay-get mir 'liny-modifi-callback)))
                   (if mf (funcall mf prim after-p mir)
                     (funcall pf prim after-p mir))))
               mirrors)
            (funcall pf prim after-p mir))))))
