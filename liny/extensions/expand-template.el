(setq liny-syntax-delimiter
      (cons
       '("\\(%\\){" . liny-expand-templ)
       liny-syntax-delimiter))

(defun liny-expand-templ-ovl (ovl)
  "liny-expand-templ-ovl is writen by ran9er"
  (if (y-or-n-p "Continues?")
      (progn
        (goto-char (overlay-end (liny-find-role ovl)))
        (liny-expand-templ (overlay-get ovl 'template)(point) ovl))
    (liny-goto-field 'nnn)))

(defun liny-expand-templ (str pos ovl)
  (overlay-put ovl 'template str)
  ;; (liny-custom-syntax "\\(\\$ovl\\)" 'liny-expand-templ-ovl)
  (let ((templ (liny-insert (liny-gen-token (overlay-get ovl 'template)) t t ovl))
        (ori (overlay-get ovl 'origin)))
    (liny-overlay-append-hooks
     (cdr templ) 'jump-relay-hooks 'liny-expand-templ-ovl)
    (overlay-put (cdr templ) 'template str)
    (if (overlay-get ovl 'next)
        (liny-overlay-link (overlay-get ovl 'previous)
                           (car templ) (cdr templ))
      (liny-overlay-push-to
       ori 'snippet-ready
       `(lambda(x)(liny-overlay-link (overlay-get ,ovl 'previous)
                                 ,(car templ) ,(cdr templ)))))))
