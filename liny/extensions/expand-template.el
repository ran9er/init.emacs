(setq liny-syntax-delimiter
      (cons
       '("\\(%\\){" . liny-expand-templ)
       liny-syntax-delimiter))

(defun liny-expand-templ-ovl (ovl)
  "liny-expand-templ-ovl is writen by ran9er"
  (if (y-or-n-p "Continues?")
      ""
    (liny-goto-field 'nnn)))

(defun liny-expand-templ (str pos ovl)
  (overlay-put ovl 'template str)
  ;; (liny-custom-syntax "\\(\\$ovl\\)" 'liny-expand-templ-ovl)
  (let ((templ (liny-insert (liny-gen-token (overlay-get ovl 'template)) t t ovl))
        (ori (overlay-get ovl 'origin)))
    (liny-overlay-append-hooks
     (cdr templ) 'jump-relay-hooks 'liny-expand-templ-ovl)
    (liny-overlay-push-to ori 'snippet-ready
     `(lambda(x)(liny-overlay-link (overlay-get ,ovl 'previous)
                               ,(car templ) ,(cdr templ))))))
