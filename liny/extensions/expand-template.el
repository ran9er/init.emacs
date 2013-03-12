(setq liny-syntax-delimiter
      (cons
       '("\\(%\\){" . liny-expand-template)
       liny-syntax-delimiter))

(defun liny-expand-template (str pos ovl)
  (overlay-put ovl 'template str)
  (let* ((ori (overlay-get ovl 'origin))
         (templ (liny-expand-templ ovl))
         (prev (overlay-get ovl 'previous)))
    (liny-overlay-push-to
     ori 'snippet-ready
     `(lambda(o)(liny-overlay-link-remove ,ovl)
        (liny-overlay-link ,prev ,(car templ) ,(cdr templ))
        (mapc (lambda(x)(overlay-put x 'origin ,ori)
                (liny-overlay-push-to ,ori 'member x))
         (overlay-get (overlay-get o 'origin) 'member))
        (liny-overlay-release (overlay-get o 'origin))))))

(defun liny-expand-templ-ovl (ovl)
  "liny-expand-templ-ovl is writen by ran9er"
  (if (y-or-n-p "Continues?")
      (progn
        (goto-char (overlay-end (liny-find-role ovl)))
        (liny-expand-templ ovl)
        (goto-char (overlay-end (overlay-get ovl 'next))))
    (liny-goto-field 'nn t)))

(defun liny-expand-templ (ov)
  "liny-expand-template is writen by ran9er"
  (let* ((str (overlay-get ov 'template))
         (templ (liny-insert (liny-gen-token str) t t ov))
         (last (cdr templ)))
    (overlay-put last 'template str)
    (liny-overlay-append-hooks
     last 'jump-relay-hooks 'liny-expand-templ-ovl)
    ;; (liny-overlay-append-hooks
    ;;  last 'jump-relay-hooks
    ;;  `(lambda(o)(liny-overlay-link o ,(car templ) ,last)))
    templ))
