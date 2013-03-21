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
         (first (car templ))
         (last (cdr templ)))
    (liny-overlay-push-to
     ori 'snippet-ready
     `(lambda(o) ;o => ori
        (liny-overlay-link-remove ,ovl)
        (liny-overlay-link ,rly ,first ,last)
        (let ((ori1 (overlay-get ,first 'origin)))
          (mapc (lambda(x)(overlay-put x 'origin ,ori)
                  (liny-overlay-push-to ,ori 'member x))
                (overlay-get ori1 'member))
          (liny-overlay-release ori1))))
    ))

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
         (templ (liny-insert (liny-gen-token str) t t nil ov)) ;templ => expand
         (ori (overlay-get ov 'origin))
         (last (cdr templ))) ;last => expand relay
    (overlay-put last 'template str)
    (liny-overlay-link ov (car templ) last)
    (mapc
     (lambda(x)(overlay-put x 'origin ori)
       (liny-overlay-push-to ori 'member x))
     (liny-ovl-get last 'origin 'member))
    (liny-overlay-append-hooks
     last 'jump-relay-hooks 'liny-expand-templ-ovl)
    templ))
