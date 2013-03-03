(setq l-snippets-syntax-delimiter
      (cons
       '("\\(%\\)" . l-snippets-dynamic-overlay)
       l-snippets-syntax-delimiter))

(defun l-snippets-dynamic-overlay (str pos ovl)
  (if (null (eq (overlay-get ovl 'role) 'mirror))
      (let ((tail (l-snippets-get-tail ovl)))
        (overlay-put tail 'insert-in-front-hooks
                     (cons 'l-snippets-ext-overlay
                           (overlay-get tail 'insert-in-front-hooks)))
        (overlay-put tail 'dynamic-trigger (read str)))
    (overlay-put ovl 'dynamic-template str)
    (l-snippets-ex-template ovl
     (l-snippets-gen-token str))))

(defun l-snippets-ext-overlay (ov after-p beg end &optional length)
  (if after-p
      (if (eq last-input-char (overlay-get ov 'dynamic-trigger))
          (let* ((ov (l-snippets-get-primary ov))
                 (b (overlay-start ov))
                 (e (overlay-end ov))
                 (o (progn
                      (l-snippets-move-overlay ov b e t)
                      (l-snippets-clone-primary ov end))))
            (l-snippets-move-overlay ov b e)
            (overlay-put (overlay-get ov 'next) 'previous o)
            (overlay-put o 'next (overlay-get ov 'next))
            (overlay-put ov 'next o)
            (overlay-put o 'previous ov)
            (overlay-put o 'ready t)
            (overlay-put ov 'face 'l-snippets-editable-face)
            (overlay-put o 'face 'l-snippets-active-face)))))

(defun l-snippets-clone-primary (ov beg)
  (let* ((ids (overlay-get ov 'id))
         (o (l-snippets-insert-field
             'primary
             ids
             (cdr
              (assoc
               (nth 1 ids)
               (l-snippets-get-snippet
                (nth 0 ids))))
             beg)))
    (mapc
     (lambda(x)
       (goto-char (overlay-end x))
       (l-snippets-overlay-push-to
        o 
        (l-snippets-insert-field 'mirror ids nil (point) o)
        'mirrors)
       (l-snippets-ex-template 
        x
        (l-snippets-gen-token 
         (overlay-get x 'dynamic-template))))
     (overlay-get ov 'mirrors))
    (goto-char (overlay-end o))
    o))


(defun l-snippets-ex-template (ov str)
  "l-snippets-expand-template"
  (l-snippets-custom-syntax "\\(\\$\\)(" (lambda(s p o)nil))
  (l-snippets-gen-token (overlay-get x 'dynamic-template))
  
  )
