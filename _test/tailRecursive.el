(defun 2list (lst)
  (if lst
      (cons
       (list (car lst)(cadr lst))
       (2list (cddr lst)))))

(defun t2list (lst acc)
  (if (caddr lst)
      (t2list (cddr lst)
              (cons (list (caddr lst)(cadddr lst)) acc ))
    acc))
(defun tlist (lst)
  (t2list lst (list (list (car lst)(cadr lst)))))

;; max-lisp-eval-depth 1000
(test-list 660 2list)
(test-list 990 tlist)
(test-times 1000 (test-list 100 2list)) ;=> 1.35
(test-times 1000 (test-list 100 tlist)) ;=> 1.42


(-
(test-times 1000000 (setq x 1000000)) ;=>1.6
(test-times 1000 (test-times 1000 (setq x 1000000))) ;=> 1.98
)   ;=> -0.36
(-
(test-times 10000 (setq x 1000000)) ;=> 0.016
(test-times 100 (test-times 100 (setq x 1000000))) ;=> 0.062
)   ;=> -0.046


