(setq liny-syntax-delimiter
      (cons
       '("\\(%\\)\\?" . liny-dynamic-overlay)
       liny-syntax-delimiter))

(defun liny-dynamic-overlay (str pos ovl)
  (if (null (eq (overlay-get ovl 'role) 'mirror))
      (let ((tail (liny-get-tail ovl)))
        ;; add ext to head of hooks, then liny-move-primary...
        (overlay-put tail 'insert-in-front-hooks
                     (cons 'liny-ext-overlay
                           (overlay-get tail 'insert-in-front-hooks)))
        (overlay-put tail 'dynamic-trigger (read str)))
    (let ((prim (overlay-get ovl 'primary))
          new)
      (liny-insert (liny-gen-token str) t))))

(defun liny-ext-overlay (ov after-p beg end &optional length)
  (if after-p
      (if (eq last-input-char (overlay-get ov 'dynamic-trigger))
          (let* ((ov (liny-get-primary ov))
                 (b (overlay-start ov))
                 (e (overlay-end ov))
                 (o (progn
                      (liny-move-overlay ov b e t)
                      (liny-clone-primary ov end))))
            (liny-move-overlay ov b e)
            (overlay-put o 'ready t)
            (overlay-put ov 'face 'liny-editable-face)
            (overlay-put o 'face 'liny-active-face)))))

    ;; (mapc
    ;;  (lambda(x)
    ;;    (goto-char (overlay-end x))
    ;;    (liny-overlay-push-to
    ;;     o 
    ;;     (car (liny-insert-field 'mirror ids nil (point) o))
    ;;     'mirrors)
    ;;    (liny-ex-template 
    ;;     x
    ;;     (liny-gen-token 
    ;;      (overlay-get x 'dynamic-template))))
    ;;  (overlay-get ov 'mirrors))

(defun liny-ex-template (ov str)
  "liny-expand-template"
  (split-string str "${mirror}")
  (liny-gen-token (overlay-get x 'dynamic-template))
  
  )
