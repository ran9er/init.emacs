;; -*- encoding: utf-8-unix; -*-
;; File-name:    <list.el>
;; Create:       <2011-12-27 21:24:57 ran9er>
;; Time-stamp:   <2012-01-20 20:47:58 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun to-alist-l (&rest lst)
  "(to-alist-l '(1 2 3 4 5 6)) => ((1 . 2) (3 . 4) (5 . 6))"
  (let* ((lst (if (listp (car lst)) (car lst) lst))
         (lst (if (eq (logand (length lst) 1) 1) `(,@lst nil) lst))
         (l (length lst))
         (new-list (cons (cons (nth (- l 2) lst)(nth (- l 1) lst)) nil))
         (cnt (1- (/ l 2))))
    (while (> cnt 0)
      (setq new-list (cons
                      (cons (nth (- l 4) lst)
                            (nth (- l 3) lst))
                      new-list)
            cnt (- cnt 1) l (- l 2)))
    new-list))

;;;###autoload
(defmacro mkal (&rest rest)
  "(mkal 1 2 3 4 5 6) => ((1 . 2) (3 . 4) (5 . 6))"
  (let* ((lst (if (eq (logand (length rest) 1) 1) `[,@rest nil] `[,@rest]))
         (l (length lst))
         (new-list (cons (cons (aref lst (- l 2))(aref lst (- l 1))) nil))
         (cnt (1- (/ l 2))))
    (while (> cnt 0)
      (setq new-list (cons
                      (cons (aref lst (- l 4))
                            (aref lst (- l 3)))
                      new-list)
            cnt (- cnt 1) l (- l 2)))
    `',new-list))

;;;###autoload
(defun zip-lists (a b)
  "(zip-lists '(1 3 5) '(2 4 6)) => ((1 . 2) (3 . 4) (5 . 6))"
  (if (and a b)
      (cons
       (cons (car a)(car b))
       (zip-lists (cdr a)(cdr b)))))

;;;###autoload
(defun merge-lists (&rest lists)
  "(merge-lists '(1 2) '(3 4) '(5 6)) => ((1 3 5) (2 4 6))"
  (let* ((l (length (car (last lists))))
         (m (1- (length lists)))
         (new-lists (zip-lists (car (last lists))(make-list l nil)))
         (lists (butlast lists)))
    (while (> m 0)
      (setq new-lists (zip-lists (car (last lists)) new-lists)
            lists (butlast lists)
            m (1- m)))
    new-lists))

;; ;;;###autoload
;; (defun add-to-list-x (LIST-VAR &rest REST)
;; "See also `add-to-list-l' `add-to-list-p'

;; \(add-to-list-x 'load-path
;;                init-dir
;;                (expand-file-name \"_misc/\" init-dir)
;;                )"
;;   (mapc (lambda(ELEMENT) (add-to-list LIST-VAR ELEMENT)) REST))

;; ;;;###autoload
;; (defun add-to-list-l (LIST-VAR LIST)
;; "See also `add-to-list-x'"
;;   (apply 'add-to-list-x LIST-VAR LIST))

;;;###autoload
(defun add-to-list-p (LIST-VAR &optional BASE &rest REST)
"See also `add-to-list-x'"
  (mapc (lambda(ELEMENT) (add-to-list LIST-VAR (expand-file-name ELEMENT BASE))) REST))
