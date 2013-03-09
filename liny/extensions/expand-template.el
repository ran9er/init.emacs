(setq liny-syntax-delimiter
      (cons
       '("\\(%\\){" . liny-expand-templ)
       liny-syntax-delimiter))

(defun liny-expand-templ-ovl (str pos ovl)
  "liny-expand-templ-ovl is writen by ran9er"
  )

(defun liny-expand-templ (str pos ovl)
  (overlay-put ovl 'template str)
  (liny-custom-syntax "\\(\\$ovl\\)" 'liny-expand-templ-ovl)
  (let ((templ (liny-insert (liny-gen-token (overlay-get ovl 'template)) t)))
    (liny-overlay-link ovl (car templ)(cdr templ)))
  )
