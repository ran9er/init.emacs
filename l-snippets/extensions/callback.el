(setq l-snippets-syntax-delimiter
      (cons
       (cons "\\(\\$\\)(" 'l-snippets-eval)
       (remove
        (assoc "\\(\\$\\)(" l-snippets-syntax-delimiter)
        l-snippets-syntax-delimiter)))

(defun l-snippets-eval (s &optional p o)
  (let ((f (read s)))
    (if (or (null (eq (nth 0 f) 'lambda))(null o))
        (eval f)
      (overlay-put o 'l-snippets-overlay-callback f)
      (overlay-put o 'modification-hooks
                   (append (overlay-get o 'modification-hooks)
                           (list 'l-snippets-call-when-modif)))
      (overlay-put o 'insert-in-front-hooks
                   (append (overlay-get o 'insert-in-front-hooks)
                           (list 'l-snippets-call-when-modif)))
      (if (overlay-get o 'tail)
          (overlay-put (overlay-get o 'tail) 'insert-in-front-hooks
                       (append (overlay-get (overlay-get o 'tail) 'insert-in-front-hooks)
                               (list 'l-snippets-call-when-modif)))))))

;; (define-key l-snippets-keymap "\C-l"
;;   (lambda()(interactive)
;;     (let* ((ov (l-snippets-get-overlay))
;;           (f (overlay-get ov 'l-snippets-overlay-callback)))
;;       (funcall f ov t))))

(defun l-snippets-call-when-modif (ov after-p beg end &optional length)
  (let ((form (overlay-get (l-snippets-get-primary ov) 'l-snippets-overlay-callback)))
    (funcall form ov after-p)))
