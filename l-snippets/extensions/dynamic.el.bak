(setq l-snippets-syntax-delimiter
      (cons
       '("\\(%\\)" . l-snippets-dynamic-overlay)
       l-snippets-syntax-delimiter))

(defun l-snippets-overlay-link (front beg end)
  (overlay-put (overlay-get front 'next) 'previous end)
  (overlay-put end 'next (overlay-get front 'next))
  (overlay-put front 'next beg)
  (overlay-put beg 'previous front))

(defun l-snippets-dynamic-overlay (str pos ovl)
  (if (null (eq (overlay-get ovl 'role) 'mirror))
      (let ((tail (l-snippets-get-tail ovl)))
        (overlay-put tail 'insert-in-front-hooks
                     (cons 'l-snippets-ext-overlay
                           (overlay-get tail 'insert-in-front-hooks)))
        (overlay-put tail 'dynamic-trigger (read str)))
    (let ((prim (overlay-get ovl 'primary))
          new)
      (l-snippets-insert (l-snippets-gen-token str) t))))

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
            (l-snippets-overlay-link ov o o)
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
    ;; (mapc
    ;;  (lambda(x)
    ;;    (goto-char (overlay-end x))
    ;;    (l-snippets-overlay-push-to
    ;;     o 
    ;;     (l-snippets-insert-field 'mirror ids nil (point) o)
    ;;     'mirrors)
    ;;    (l-snippets-ex-template 
    ;;     x
    ;;     (l-snippets-gen-token 
    ;;      (overlay-get x 'dynamic-template))))
    ;;  (overlay-get ov 'mirrors))
    (goto-char (overlay-end o))
    o))


(defun l-snippets-ex-template (ov str)
  "l-snippets-expand-template"
  (split-string str "${mirror}")
  (l-snippets-gen-token (overlay-get x 'dynamic-template))
  
  )
