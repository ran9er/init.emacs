(file-name-directory load-file-name)
(setq xxx load-file-name)





(add-hook 'find-file-hook
          '(lambda ()
             (let* ((bf (buffer-name))
                    (mode
                     (catch 'md
                       (mapcar (lambda (x)(and
                                       (string-match (car x) bf)
                                       (throw 'md (symbol-name (cdr x)))))
                               auto-mode-alist))))
               (load
                (gethash mode *auto-hook-hash*
                         (make-temp-name ""))
                '(a
                  b
                  c
                  d)
                t))))

