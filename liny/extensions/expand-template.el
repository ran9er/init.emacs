(setq liny-syntax-delimiter
      (cons
       '("\\(%\\){" . liny-expand-template)
       liny-syntax-delimiter))

(defun liny-expand-template (str pos ovl)
  (let* ((ori (overlay-get ovl 'origin)) ;ovl => prim
         (role (liny-ovl-get ovl 'role))
         (new (liny-insert-field   ;rly => relay
               'relay nil nil nil ori ovl))
         (rly (car new))
         ;; (void (liny-overlay-link ovl rly))
         (void (overlay-put rly 'template str))
         (templ (liny-expand-templ
                 rly
                 (if (eq role 'mirror) ori)))
         (first (nth 0 templ))
         (last (nth 1 templ)))
    (if (eq role 'primary)
        (liny-overlay-push-to
         ori 'snippet-ready
         `(lambda(o) ;o => ori
            (liny-overlay-link ,ovl ,first ,last)
            (liny-overlay-link-remove ,ovl)
            )))))

(defun liny-expand-templ-ovl (ovl)
  "liny-expand-templ-ovl is writen by ran9er"
  (if (y-or-n-p "Continues?")
      (let (new n1)
        (goto-char (overlay-end (liny-find-role ovl)))
        (setq new (liny-expand-templ ovl))
        (goto-char (overlay-end (nth 0 new)))
        (setq n1 (overlay-end (nth 1 new)))
        (move-overlay (liny-ovl-get ovl 'origin 'end)
                      n1 n1))
    (let ((o (liny-get-primary (liny-get-overlay))))
      (goto-char (overlay-end (liny-ovl-get o 'origin 'end)))
      (liny-clear-instance o))))

(defun liny-expand-templ (ov &optional origin)
  "liny-expand-template is writen by ran9er"
  (let* ((str (overlay-get ov 'template)) ;ov => relay
         (templ (liny-insert-snippet (liny-gen-token str) t t origin ov)) ;templ => expand
         (ori (overlay-get ov 'origin))
         (last (nth 1 templ)) ;last => expand relay
         (ori1 (liny-ovl-get last 'origin)))
    (overlay-put last 'template str)
    (liny-overlay-link ov (car templ) last)
    (mapc
     (lambda(x)(overlay-put x 'origin ori)
       (liny-overlay-push-to ori 'member x))
     (liny-ovl-get ori1 'origin 'member))
    (liny-overlay-release ori1)
    (liny-overlay-append-hooks
     last 'jump-relay-hooks 'liny-expand-templ-ovl)
    templ))
