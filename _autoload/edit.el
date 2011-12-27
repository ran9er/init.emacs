;; -*- encoding: utf-8-unix; -*-
;; File-name:    <edit.el>
;; Create:       <2011-12-27 21:29:35 ran9er>
;; Time-stamp:   <2011-12-27 23:23:52 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
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


;;;###autoload
(defun resize-horizontal-space (&optional backward-only)
  (interactive "*P")
  (let (fwd-pos bwd-pos
        (orig-pos (point)))
    (setq
     fwd-pos (progn (skip-chars-forward " \t") (eolp))
     bwd-pos (progn (skip-chars-backward " \t") (bolp)))
    (goto-char orig-pos)
    (if (or fwd-pos bwd-pos)
        (delete-horizontal-space backward-only)
      (delete-horizontal-space backward-only)
      (insert " "))))

;; parallel-edit
(defun insert-char-from-read(c)
  (cond
   ((eq c 13)
    (newline))
   ((eq c 127)
    (delete-backward-char 1))
   ((eq c 23)
    (backward-kill-word 1))
   (t
    (insert-char c 1))))

(defun mirror-region (src psn mkr-lst)
  "mirror region in mkr-lst, with str, and goto psn"
  (let ((str (buffer-substring-no-properties (car src)(cdr src))))
    (mapcar
     (lambda(x)
       (delete-region (car x)(1- (cdr x)))
       (goto-char (car x))
       (insert str))
     mkr-lst))
  (goto-char psn))

;;;###autoload
(defun parallel-edit (position-list &optional prt)
  (interactive)
  (let* ((p (or prt 0))
         (start-position (point-marker))
         end-position y x
         (end-marker (progn (forward-char (1+ p))(point-marker)))
         (marker-list (mapcar (lambda (x)
                                (cons
                                 (progn (goto-char x)(point-marker))
                                 (progn (forward-char (1+ p))(point-marker))))
                              position-list)))
    (goto-char start-position)
    (setq y nil)
    (while (null (eq (setq x (read-char "parallel-edit")) 13))
      (if y nil
        (delete-region start-position (1- end-marker)))
      (insert-char-from-read x)
      (setq end-position (1- end-marker)
            y t)
      (mirror-region (cons start-position end-position) end-position marker-list))))

