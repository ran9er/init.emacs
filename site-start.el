;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (boundp '*init-dir*);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let ((init-name-match "init.*emacs\\|emacs.*init")
        (base-dir
         (apply 'expand-file-name
                (if (eq system-type 'windows-nt)
                    (list ".." exec-directory)
                  (list "~" nil)))))
    (setq *init-dir*
          (file-name-as-directory
           (or
            (car
             (sort
              (mapcar (lambda(x) (if (file-directory-p x) x (make-temp-name "")))
                      (directory-files base-dir t init-name-match t))
              'file-newer-than-file-p))
            (make-temp-name "")))))

  (if (file-exists-p *init-dir*)
      (progn
        (setq *init-time* (cons (float-time)()))

        (mapc (lambda (init-load-path)
                (if (file-directory-p init-load-path)
                    (add-to-list 'load-path init-load-path)))
              (cons *init-dir* (directory-files *init-dir* t "^_")))

        (setq *init-files* (mapc 'load (directory-files *init-dir* t "\\.el\\'")))

        (setcdr *init-time* (float-time))

        (add-hook
         'emacs-startup-hook
         '(lambda ()
            (message "load %d init file , spend %g seconds ; startup spend %g seconds"
                     (length *init-files*)
                     (- (cdr *init-time*) (car *init-time*))
                     (- (float-time) (car *init-time*)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
