(defun 2list (lst)
  (if lst
      (cons
       (cons (car lst)(cadr lst))
       (2list (cddr lst)))))

(defun t2list (lst acc)
  (if (caddr lst)
      (t2list (cddr lst)
              (cons (cons (caddr lst)(cadddr lst)) acc ))
    acc))
(defun tlist (lst)
  (t2list lst (list (cons (car lst)(cadr lst)))))

;; max-lisp-eval-depth 1000
(test-list 662 2list)
(test-list 992 tlist)
(tlist (mklst 992))
(2list (mklst 662))
(test-times 1000 (test-list 100 2list)) ;=> 0.95
(test-times 1000 (test-list 100 tlist)) ;=> 1.0


(-
(test-times 1000000 (setq x 1000000)) ;=>1.6
(test-times 1000 (test-times 1000 (setq x 1000000))) ;=> 1.98
)   ;=> -0.36
(-
(test-times 10000 (setq x 1000000)) ;=> 0.016
(test-times 100 (test-times 100 (setq x 1000000))) ;=> 0.062
)   ;=> -0.046
