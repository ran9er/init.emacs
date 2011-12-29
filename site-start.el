(unless (boundp '*init-dir*)

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
              (mapcar (lambda (x) (if (file-directory-p x) x (make-temp-name "")))
                      (directory-files base-dir t init-name-match t))
              'file-newer-than-file-p))
            (make-temp-name ""))))
         (init-files
          (mapcar
           (lambda (f) (file-name-sans-extension f))
           (directory-files init-dir t "\\.el\\'"))))

    (when (file-exists-p init-dir)

      (setq init-time (list (float-time)))

      ;; export *init-dir*
      (defvar *init-dir* init-dir)

      ;; add "_xxx_" to load-path
      (mapc (lambda (p)
              (if (file-directory-p p)
                  (add-to-list 'load-path p)))
            (directory-files init-dir t "^_.*_\\'"))

      ;; autoload
      (funcall
       (lambda (dir &optional f loaddefs basedir)
         (let* ((path
                 (expand-file-name dir (or basedir *init-dir*)))
                (ldfs
                 (or loaddefs (expand-file-name "_loaddefs" path)))
                index force)
           (setq index
                 (funcall
                  (lambda (dir)
                    (let (out i p)
                      (with-temp-buffer
                        (insert-file-contents dir)
                        (setq p (point-max))
                        (while (setq i (search-forward-regexp ";;; Generated autoloads from " p t))
                          (setq out (cons (buffer-substring-no-properties i (line-end-position))
                                          out)))) out))
                  ldfs))
           (setq force (or f (null (equal (reverse index)
                                          (directory-files path nil "\\.el\\'")))))
           (if force (delete-file ldfs))
           (mapcar
            (lambda (fl)
              (if (or force
                      (null (file-newer-than-file-p ldfs fl)))
                  (update-file-autoloads fl t ldfs)))
            (directory-files path t "\\.el\\'"))
           (load ldfs)))
       "_autoload_/")

      ;; auto-hook-alist
      (defvar auto-hook-alist
        (mapcar
         (lambda (x)
           (cons (file-name-sans-extension (file-name-nondirectory x)) x))
         (directory-files (expand-file-name "_extensions/" init-dir) t "\\.el\\'")))

      (add-hook 'find-file-hook
                '(lambda ()
                   (let (mode)
                     (mapcar (lambda (x)
                               (and
                                (string-match (car x)(buffer-name))
                                (setq mode (symbol-name (cdr x)))))
                             auto-mode-alist)
                     ;; (setq mode
                     ;;       (or mode
                     ;;           (and (string-equal "*" (substring (buffer-name) 0 1))
                     ;;                (substring (buffer-name) 1 -1))))
                     (load (or
                            (cdr (assoc mode auto-hook-alist))
                            (make-temp-name ""))
                           t))))

      ;; byte-compile
      (when nil
        ;; delete elc without el
        (mapc (lambda(f)(or (file-exists-p (substring f 0 -1))
                        (delete-file f)))
              (directory-files init-dir t "\\.elc\\'"))
        ;; recompile
        (eval-when-compile (require 'bytecomp))
        (mapc (lambda(f) (byte-recompile-file f nil 0))
              (directory-files init-dir t "\\.el\\'")))

      ;; load *.elc || *.el in init-dir
      (defvar *init-time* nil)
      (let (tm)
        (mapc
         (lambda (f)
           (setq tm (float-time))
           (load f)
           (setq *init-time*
                 (cons
                  (cons
                   (file-name-nondirectory f)
                   (- (float-time) tm))
                  *init-time*)))
         init-files))

      (setcdr init-time (float-time))

      (setq *init-time*
            (cons
             (cons "Total"
                   (- (cdr init-time)
                      (car init-time)))
             *init-time*))

      ;; when init finished, echo some info
      (add-hook
       'emacs-startup-hook
       `(lambda ()
          (message "load %d init file , spend %g seconds ; startup spend %g seconds"
                   (1- (length *init-time*))
                   (cdar *init-time*)
                   (- (float-time) ,(car init-time))))))))
