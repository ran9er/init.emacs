(setq l-snippets-syntax-delimiter
      (cons
       '("%" l-snippets-dynamic-overlay)
       l-snippets-syntax-delimiter))

(defun l-snippets-dynamic-overlay (str pos ovl)
  (let ((tail (l-snippets-get-tail ovl)))
    (if tail
        (progn 
          (overlay-put tail 'insert-in-front-hooks '(l-snippets-ext-overlay))
          (overlay-put tail 'dynamic-tigger (read arg)))
      (overlay-put ovl 'ext-templ arg))))
(defun l-snippets-ext-overlay ()
  (if (eq last-input-char ?,)
      (let* ((id (read (make-temp-name "dy-")))
             (p (point))
             ;; clone overlay
             (ov (l-snippets-overlay-appoint 'primary p p 'id id)))
        ;; (move-overlay )
        "insert ov to instence lst after dynamic-overlay"
        ;; insert mirror
        )))
