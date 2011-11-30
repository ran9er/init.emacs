;; In sandbox
(defun mylist (lst)
  (if (car lst)
      (cons (car lst) (mylist (cdr lst)))
    nil))
(defun mlist (&rest lst)
  (mylist lst))
(mylist '(a b c d e)) ;=> (a b c d e)
(mylist '(a . (b . (c . (d . (e . nil)))))) ;=> (a b c d e)
(mylist '(a . b)) ;=> xxx
(equal (mklst 3) '("1" . ("2" . ("3" . nil)))) ;=>t
(mlist 'a 'b 'c 'd 'e)
