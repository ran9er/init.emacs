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
  (let (k v result tmp)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward sep nil t)
        (setq k (cons
                 (cons
                  (match-beginning 0)
                  (match-end 0))
                 k)))
      (setq k (reverse k)
            tmp k)
      (while tmp
        (setq v (cons (caar tmp) v)
              v (cons (cdar tmp) v))
        (setq tmp (cdr tmp)))
      (setq v (cons (point-max) v)
            v (reverse v)
            v (cdr v)
            v (l-snippets-to-alist v))
      (while k
        (setq
         result
         (cons
          (cons
           (buffer-substring-no-properties (caar k) (cdar k))
           (buffer-substring-no-properties (caar v) (cdar v)))
          result))
        (setq k (cdr k)
              v (cdr v))))
    result))
