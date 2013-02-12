(defvar skeleton-pair-cond-alist)
(setq skeleton-pair-cond-alist
  '(
    ((char-bf ?=) . (?\{ _ "}"))
    ((char-bf '(?+ ?-)) . (?\\ "+"  _  "+\\"))
    ((or (char-bf ?/)(char-bf ?=)) . (?\[ n _ n "]"))
    ((bolp) . (?/ "*" n  _  n "*/"))
    (t . (?/ _))
    ))
(defadvice skeleton-pair-insert-maybe (around xxx activate)
  (let ((skeleton-pair-alist skeleton-pair-alist)
        (x skeleton-pair-cond-alist))
    (while
        (and
         x
         (null
          (if (and
               (eq last-command-event (cadr (car x)))
               (eval (caar x)))
              (setq skeleton-pair-alist (list (cdar x)))
            nil)))
      (setq x (cdr x)))
    ad-do-it))
;; (defadvice skeleton-pair-insert-maybe (around xxx activate)
;;   (let ((skeleton-pair-alist skeleton-pair-alist))
;;     (mapc
;;      (lambda(x)
;;        (if (and (eq last-command-event (cadr x))(eval (car x)))
;;            (setq skeleton-pair-alist (list (cdr x)))))
;;      skeleton-pair-cond-alist)
;;     ad-do-it))
(defun char-bf (x)
  (let ((x (if (listp x) x (list x))))
    (save-excursion
      (skip-chars-backward " \t")
      (memq (char-before (point)) x))))

;;;###autoload
(defun skeleton-pair-alist-update ()
  (interactive)
  (mapcar
   (lambda(x)
     ;; (local-set-key (char-to-string (cadr x)) 'skeleton-pair-insert-maybe)
     (define-key (current-local-map)
       (eval `(kbd ,(char-to-string (cadr x))))
       'skeleton-pair-insert-maybe))
   skeleton-pair-cond-alist))
;; (skeleton-pair-alist-update)

