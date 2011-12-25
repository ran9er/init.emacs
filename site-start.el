;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (boundp '*init-dir*);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* (init-time
         (init-name-match "init.*emacs\\|emacs.*init")
         (base-dir
          (apply 'expand-file-name
                 (if (eq system-type 'windows-nt)
                     (list ".." exec-directory)
                   (list "~" nil))))
         (init-dir
          (file-name-as-directory
           (or
            (car
             (sort
              (mapcar (lambda(x) (if (file-directory-p x) x (make-temp-name "")))
                      (directory-files base-dir t init-name-match t))
              'file-newer-than-file-p))
            (make-temp-name ""))))
         (init-files
          (directory-files init-dir t "\\.el\\'")))

    (when (file-exists-p init-dir)

      ;; export *init-dir*
      (setq *init-dir* init-dir)

      ;; delete elc without el
      (mapc (lambda(f)(or (file-exists-p (substring f 0 -1))
                      (delete-file f)))
            (directory-files init-dir t "\\.elc\\'"))

      ;; recompile
      (eval-when-compile (require 'bytecomp))
      (mapc (lambda(f) (byte-recompile-file f nil 0))
            (directory-files init-dir t "\\.el\\'"))

      (setq init-time (cons (float-time)()))

      ;; add init-dir & dir "^_" to load-path
      (mapc (lambda (p)
              (if (file-directory-p p)
                  (add-to-list 'load-path p)))
            (cons init-dir (directory-files init-dir t "^_")))

      ;; load *.elc || *.el in init-dir
      (mapc (lambda (f) (load (file-name-sans-extension f))) init-files)

      (setcdr init-time (float-time))

      ;; when init finished, echo some info
      (add-hook
       'emacs-startup-hook
       `(lambda ()
          (message "load %d init file , spend %g seconds ; startup spend %g seconds"
                   ,(length init-files)
                   ,(- (cdr init-time) (car init-time))
                   (- (float-time) ,(car init-time)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  );;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
