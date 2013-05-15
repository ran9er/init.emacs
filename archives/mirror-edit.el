(defun mirror-append (&rest position-list)
  (interactive)
  (let ((start-position (point))
        move-position move-position-acc
        insert-position)
    (while (setq insert-position (read-char "mirror edit"))
      (insert-char-from-read insert-position)
      (setq move-position (- (point) start-position)
            move-position-acc move-position)
      (mapcar
       (lambda(x)
         (goto-char (+ x move-position-acc move-position))
         (if (eq insert-position 127) (forward-char 2))
         (insert-char-from-read insert-position)
         (setq move-position-acc (+ move-position-acc move-position)))
       position-list)
      (goto-char (+ start-position move-position)))))


(defun serial-edit (&rest position-list)
  )

(lambda()
   (interactive)(let ((x (point))) (parallel-edit  (list (+ 1 x) (+ 2 x)) )))
<TAG>xml</TAG>
(lambda()(goto-char 854)(parallel-edit '(863) 3))
(setq foo (make-overlay 854 863))
(overlay-properties foo)
(overlay-put foo 'face `(ground-color . ,(face-attribute 'font-lock-warning-face :foreground)))











;; * parallel-edit
(defun parallel-edit (position-list &optional prt)
  (interactive)
  (let* ((p (or prt 0))
         (start-position (point-marker))
         end-position mirror x
         (end-marker (progn (forward-char (1+ p))(point-marker)))
         (ovl (make-overlay start-position (1- end-marker)))
         (marker-list (mapcar (lambda (x)
                                (cons
                                 (progn (goto-char x)(point-marker))
                                 (progn (forward-char (1+ p))(point-marker))))
                              position-list))
         (inhibit-modification-hooks t)
         (y nil))
    (goto-char start-position)
    (overlay-put ovl 'face
                 `(foreground-color . ,(face-attribute 'font-lock-warning-face :foreground)))
    (overlay-put ovl 'modification-hooks
                 '((lambda(ovl t start-position end-position)
                     (mirror-region (cons start-position (1- end-marker)) end-position marker-list)
                                             )))
    (while (null (eq (setq x (read-char "parallel-edit")) 13))
      (if y nil
        (delete-region start-position (1- end-marker)))
      (insert-char-from-read x)
      (setq y t)
      )
    (delete-overlay ovl)))

