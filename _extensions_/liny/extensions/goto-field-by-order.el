(defvar liny-goto-field-func 'liny-goto-field-by-order)

(defun liny-sort-field (ov)
  "liny-sort-field is writen by ran9er"
  (let* (lst
         (o (overlay-get ov 'origin))
         (1st (liny-get-first ov))
         (go (lambda(ov)
               (if (null ov) nil
                 (setq lst (cons (cons (nth 1 (overlay-get ov 'id)) ov) lst))
                 (overlay-put ov 'sorted t)
                 (funcall go (overlay-get ov 'next))))))
    (funcall go 1st)
    (sort lst (lambda(x y)(<= (car x)(car y))))
    (if (eq 0 (caar lst))
        (setq lst (append (cdr lst)(list (car lst)))))
    (overlay-put o 'field-order lst)))

(add-hook 'liny-overlay-release-hook
          (lambda(o)
            (if (overlay-get o 'sorted)
                (overlay-put o 'sorted nil))))

(defun liny-goto-field-by-order (p-or-n)
  (interactive)
  (let* ((o (liny-get-primary (liny-get-overlay)))
         (ori (overlay-get o 'origin))
         (lst (if (overlay-get o 'sorted)
                  (overlay-get ori 'field-order)
                (liny-sort-field o)))
         (idx (funcall
               (cond ((eq p-or-n 'next) '1+)
                     ((eq p-or-n 'previous) '1-))
               (- (length lst)
                  (length (member (rassq o lst)
                                  lst)))))
         (oo (if (< idx 0) nil (cdr (nth idx lst)))))
    (liny-goto-field-really oo)))
