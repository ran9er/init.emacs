;; -*- encoding: utf-8-unix; -*-
;; File-name:    <element-of.el>
;; Create:       <2012-01-20 13:29:51 ran9er>
;; Time-stamp:   <2012-01-21 11:04:11 ran9er>
;; Mail:         <2999am@gmail.com>
(defmacro elf (&rest form)
  (let* ((result (gensym))
         (iter-lst '(being in on
                     from downfrom upfrom
                     to upto below downto above))
         (verb-lst '(collect append nconc
                     count sum maximize minimize
                     collecting appending nconcing
                     counting summing maximizing minimizing))
         (search
          (lambda (p o)
            (car
             (sort
              (remq nil (mapcar (lambda(x)(memq x o)) p))
              (lambda(x y)(> (length x)(length y)))))))
         (iter (funcall search iter-lst form))
         (form (butlast form (length iter)))
         (verb (or (car (funcall search verb-lst form)) 'collect))
         (this (gensym))
         (tmp (car (remq verb form)))
         (pred (if tmp (funcall tmp this) this)))
    `(loop for ,this ,@iter
           ,verb ,pred into ,result
           finally (return ,result))))

(defmacro elf (&rest form)
  "&optional verb pred &rest form
(elf upto 10) => (0 1 2 3 4 5 6 7 8 9 10)
(elf x upto 10) => (0 1 2 3 4 5 6 7 8 9 10)
(elf 1+ upto 10) => (1 2 3 4 5 6 7 8 9 10 11)
(elf (+ 100 x) upto 10) => (100 101 102 103 104 105 106 107 108 109 110)
(elf (lambda(x)(+ 100 x)) upto 10) => (100 101 102 103 104 105 106 107 108 109 110)
(elf sum upto 10) => 55
(elf x sum upto 10) => 55
(elf 1+ sum upto 10) => 66
(elf (+ 100 x) sum upto 10) => 1155
(elf (lambda(x)(+ 100 x)) sum upto 10) => 1155
"
  (let* ((result (gensym))
         (iter-lst '(being in on
                     from downfrom upfrom
                     to upto below downto above))
         (verb-lst '(collect append nconc
                     count sum maximize minimize
                     collecting appending nconcing
                     counting summing maximizing minimizing))
         ;; safe
         (search
          (lambda (p o)
            (car
             (sort
              (remq nil (mapcar (lambda(x)(memq x o)) p))
              (lambda(x y)(> (length x)(length y)))))))
         ;; fast
         ;; (search
         ;;  (lambda (p o)
         ;;    (catch 'return
         ;;      (remq nil
         ;;            (mapcar
         ;;             (lambda(x)(let ((m (memq x o)))(and m (throw 'return m))))
         ;;             p)))))
         (filter
          (lambda(i)(car (remq
                      nil
                      (mapcar
                       (lambda(x)
                         (and
                          (atom x)
                          (null (memq x '(lambda or and)))
                          (null (functionp x))
                          (symbolp x)
                          x)) i)))))
         (iter (funcall search iter-lst form))
         (form (butlast form (length iter)))
         (verb (or (car (funcall search verb-lst form)) 'collect))
         (tmp (gensym))
         (pred (or (car (remq verb form)) tmp))
         this)
    (setq this
          (cond
           ((functionp pred)
            (prog1 tmp
              (setq pred
                    `(funcall ',pred ,tmp))))
           ((symbolp pred)
            pred)
           ((listp pred)
            (funcall filter pred))))
    `(loop for ,this ,@iter
           ,verb ,pred into ,result
           finally (return ,result))))

(defmacro ele0 (&rest form)
  (let* (verb pred rest
         (arg1 (nth 0 form))
         (arg2 (nth 1 form))
         (i (gensym))
         (r (gensym))
         (verb-lst '(collect
                     append
                     nconc
                     count
                     sum
                     maximize
                     minimize)))
    (setq verb arg1 pred arg2 rest (cddr form))
    `(loop for ,i ,@rest
           ,verb (funcall ,pred ,i)
           into ,r
           finally (return ,r))))

(defmacro ele1 (collect &rest form)
  "(ele (number-to-string i) upto 10) => (\"0\" \"1\" \"2\" \"3\" \"4\" \"5\" \"6\" \"7\" \"8\" \"9\" \"10\")"
  `(loop for i ,@form collect ,collect))

(defmacro ele (&rest form)
  (let* ( ;; (i (gensym))
         (this 'i)
         (pred (nth 0 form))
         (verb (nth 1 form))
         (rest (nthcdr 2 form))
         (r (gensym))
         (verb-lst '(collect
                     append
                     nconc
                     count
                     sum
                     maximize
                     minimize)))
    (if (null (memq verb verb-lst))
        (setq verb 'collect
              rest (cdr form)))
    `(loop for ,this ,@rest
           ,verb ,pred into ,r
           finally (return ,r))))
