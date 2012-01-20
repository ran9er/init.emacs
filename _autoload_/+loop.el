;; -*- encoding: utf-8-unix; -*-
;; File-name:    <+loop.el>
;; Create:       <2012-01-20 12:17:26 ran9er>
;; Time-stamp:   <2012-01-21 01:25:20 ran9er>
;; Mail:         <2999am@gmail.com>

;; * element-of

;;;###autoload
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
              (lambda(x y)(if (> (length x)(length y)) x y))))))
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
