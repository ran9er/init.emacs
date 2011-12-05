;; -*- encoding: utf-8-unix; -*-
;; File-name:    <20111130150308-2420CEn.el>
;; Create:       <2011-11-30 15:03:08 ran9er>
;; Time-stamp:   <2011-12-06 00:14:22 ran9er>
;; Mail:         <2999am@gmail.com>
(defun swap-point()
  (interactive)
  (if (or (null (boundp '*last-point*)) (null *last-point*))
      (progn (make-local-variable '*last-point*)
             (setq *last-point* (cons (point) (point))))
    (let ((p (point)))
      (if (eq p (cdr *last-point*))
          (progn (goto-char (car *last-point*))
                 (setq *last-point* (cons (cdr *last-point*)(car *last-point*))))
        (goto-char (cdr *last-point*))
        (setq *last-point* (cons p (cdr *last-point*)))))))

(defun jump-point(&optional n)
  (interactive "P")
  (if (>= n 2)
      (setq *recent-point* (butlast *recent-point* (- (length *recent-point*) n)))
      (if (or (null (boundp '*recent-point*)) (null *recent-point*))
          (progn (make-local-variable '*recent-point*)
                 (setq *recent-point* (list (point)(point))))
        (let ((p (point)) l)
          (if (setq l (memq p *recent-point*))
              (progn (goto-char (cadr l))
                     (setq *recent-point* (append l (butlast *recent-point* (length l)))))
            (goto-char (cadr *recent-point*))
            (if (eq (car *recent-point*)(cadr *recent-point*))
                (setcar *recent-point* p)
              (setq *recent-point* (cons p *recent-point*))))))))

(setq *recent-point* nil)
(jump-point)


