;;;###autoload
(defun search-str (regexp str &rest sub)
  (let (result)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq
         result
         (cons
          (car
           (mapcar (lambda(x)(list
                          (match-string x)
                          (match-beginning x)
                          (match-end x))) sub))
          result))))
    result))

;;;###autoload
(defun split-str (sep str)
  (let (k result)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward sep nil t)
        (setq k (cons
                 (match-end 0)
                 (cons
                  (match-end 0)
                  (cons
                   (match-beginning 0)
                   (cons
                    (match-beginning 0)
                    k))))))
      (setq k (to-alist
               (to-alist
                (cdr (reverse (cons (point-max) k))))))
      (mapcar
       (lambda(x)
         (cons (buffer-substring-no-properties
                (car (car x)) (cdr (car x)))
               (buffer-substring-no-properties
                (car (cdr x)) (cdr (cdr x)))))
       k))))
