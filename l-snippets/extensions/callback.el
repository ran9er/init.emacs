;; (define-key l-snippets-keymap "\C-l"
;;   (lambda()(interactive)
;;     (let* ((ov (l-snippets-get-overlay))
;;           (f (overlay-get ov 'l-snippets-modifi-callback)))
;;       (funcall f ov t))))

(setq l-snippets-syntax-delimiter
      (cons
       (cons "\\(\\$\\)(" 'l-snippets-eval)
       (remove
        (assoc "\\(\\$\\)(" l-snippets-syntax-delimiter)
        l-snippets-syntax-delimiter)))

(defun l-snippets-eval (s &optional p o)
  (let ((f (read s)))
    (if (and (listp f)(null (functionp f)))
        (eval
         `(let ((str ,s)(pos ,p)(ovl ,o)) ,f))
      (overlay-put o 'l-snippets-modifi-callback f)
      (l-snippets-primary-append-hooks o 'l-snippets-call))))

(defun l-snippets-call (ov after-p beg end &optional length)
  (let* ((prim (l-snippets-get-primary ov))
         (pf (overlay-get prim 'l-snippets-modifi-callback)))
    (if (overlay-get prim 'ready)
        (let ((mirrors (overlay-get prim 'mirrors)))
          (if mirrors
              (mapc
               (lambda(mir)
                 (let ((mf (overlay-get mir 'l-snippets-modifi-callback)))
                   (if mf (funcall mf prim after-p mir)
                     (funcall pf prim after-p mir))))
               mirrors)
            (funcall pf prim after-p mir))))))
