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
          (directory-files init-dir t "\\.el\\'")))

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
      (defun autoload-directory (dir &optional force loaddefs basedir)
        (let* ((path
                (expand-file-name dir (or basedir *init-dir*)))
               (ldfs
                (or loaddefs (expand-file-name "_loaddefs" path))))
          (if force (delete-file ldfs))
          (mapcar
           (lambda (f)
             (if (or force
                     (null (file-newer-than-file-p ldfs f)))
                 (update-file-autoloads f t ldfs)))
           (directory-files path t "\\.el\\'"))
          (load ldfs)))

      (autoload-directory "_autoload_/")

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
      (mapc (lambda (f) (load (file-name-sans-extension f))) init-files)

      (setcdr init-time (float-time))

      ;; when init finished, echo some info
      (add-hook
       'emacs-startup-hook
       `(lambda ()
          (message "load %d init file , spend %g seconds ; startup spend %g seconds"
                   ,(length init-files)
                   ,(- (cdr init-time) (car init-time))
                   (- (float-time) ,(car init-time))))))))
