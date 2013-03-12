(setq liny-syntax-delimiter
      (cons
       '("\\(%\\){" . liny-expand-template)
       liny-syntax-delimiter))

(defun liny-expand-template (str pos ovl)
  (let* ((ori (overlay-get ovl 'origin)) ;ovl => prim
         (rly (car (liny-insert-field   ;rly => relay
                    'relay nil nil nil ori)))
         (void (liny-overlay-link ovl rly))
         (void (overlay-put rly 'template str))
         (templ (liny-expand-templ rly))
         (first (car templ)))
    (liny-overlay-push-to
     ori 'snippet-ready
     `(lambda(o)(liny-overlay-link-remove ,ovl) ;o => ori
        (liny-overlay-link ,rly ,first ,(cdr templ))
        (mapc (lambda(x)(overlay-put x 'origin ,ori)
                (liny-overlay-push-to ,ori 'member x))
              (overlay-get (overlay-get ,first 'origin) 'member))
        (liny-overlay-release (overlay-get ,first 'origin))))))

(defun liny-expand-templ-ovl (ovl)
  "liny-expand-templ-ovl is writen by ran9er"
  (if (y-or-n-p "Continues?")
      (progn
        (goto-char (overlay-end (liny-find-role ovl)))
        (goto-char (overlay-end (car (liny-expand-templ ovl)))))
    (liny-goto-field 'nn t)))

(defun liny-expand-templ (ov)
  "liny-expand-template is writen by ran9er"
  (let* ((str (overlay-get ov 'template)) ;ov => relay
         (templ (liny-insert (liny-gen-token str) t t ov)) ;templ => expand
         (last (cdr templ))) ;last => expand relay
    (overlay-put last 'template str)
    (liny-overlay-link ov (car templ) last)
    (liny-overlay-append-hooks
     last 'jump-relay-hooks 'liny-expand-templ-ovl)
    templ))
