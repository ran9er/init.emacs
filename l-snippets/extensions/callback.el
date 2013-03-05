(setq l-snippets-syntax-delimiter
      (cons
       (cons "\\(\\$\\)(" 'l-snippets-eval)
       (remove
        (assoc "\\(\\$\\)(" l-snippets-syntax-delimiter)
        l-snippets-syntax-delimiter)))

(defun l-snippets-eval (s &optional p o)
  (let ((f (read s)))
    (if (and (listp f)(null (functionp f)))
        (eval f)
      (overlay-put o 'l-snippets-overlay-callback f)
      (l-snippets-primary-append-hooks o 'l-snippets-call-when-modif))))

;; (define-key l-snippets-keymap "\C-l"
;;   (lambda()(interactive)
;;     (let* ((ov (l-snippets-get-overlay))
;;           (f (overlay-get ov 'l-snippets-overlay-callback)))
;;       (funcall f ov t))))

(defun l-snippets-call-when-modif (ov after-p beg end &optional length)
  (let* ((o (l-snippets-get-primary ov))
         (form (overlay-get o 'l-snippets-overlay-callback)))
    (funcall form o after-p)))
