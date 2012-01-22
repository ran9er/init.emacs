(defvar *init-status*
  ;;;;;;;;;;
  (catch 'quit
    ;;;;;;;;
    (if (boundp '*init-time*)
        (throw 'quit "have been loaded"))
    ;;;;;;;;
    (let ((this-file (file-name-nondirectory load-file-name))
          (this-dir (file-name-directory load-file-name))
          init-name-match base-dir init-dir init-files)
      ;;;;;;
      (cond
       ((boundp '*init-dir*) nil)
       ((member load-file-name
                (mapcar 'expand-file-name
                        (list "~/.emacs"
                              (locate-library site-run-file))))
        (setq
         init-name-match
         "init.*emacs\\|emacs.*init"
         base-dir
         (apply 'expand-file-name
                (cond
                 ((eq system-type 'windows-nt)
                  `(".." ,exec-directory))
                 (t
                  `("~"))))
         init-dir
         ((lambda (x) (file-name-as-directory
                   (or (car (sort x 'file-newer-than-file-p))
                       (make-temp-name ""))))
          (mapcar (lambda (x) (if (file-directory-p x) x (make-temp-name "")))
                  (directory-files base-dir t init-name-match t)))))
       (t (setq init-dir this-dir)))
      ;; export *init-dir*
      (defvar *init-dir* init-dir)
      ;;;;;;
      (if (null (file-exists-p *init-dir*))
          (throw 'quit "can't found *init-dir*"))
      ;;;;;;
      (setq init-files
            (mapcar
             (lambda (f) (file-name-sans-extension f))
             (directory-files *init-dir* t "\\.el\\'")))
      ;; add "_xxx_" to load-path
      (mapc
       (lambda (p)
         (if (file-directory-p p)
             (add-to-list 'load-path p)))
       (directory-files *init-dir* t "^_.*_\\'"))
      ;; autoload
      ((lambda (dir &optional loaddefs basedir)
         (let* ((path
                 (expand-file-name dir (or basedir *init-dir*)))
                (ldfs
                 (or loaddefs "_loaddefs"))
                (generated-autoload-file
                 (expand-file-name ldfs path)))
           (update-directory-autoloads path)
           (kill-buffer ldfs)
           (load generated-autoload-file)))
       "_autoload_/")
      ;; *auto-hook-hash*
      (defvar *auto-hook-hash* (make-hash-table :test 'equal :size 20))
      (mapc
       (lambda (x)
         (puthash
          (file-name-sans-extension (file-name-nondirectory x)) x
          *auto-hook-hash*))
       (directory-files (expand-file-name "_extensions/" *init-dir*) t "\\.el\\'"))
      ;;;;;;
      (add-hook 'find-file-hook
                '(lambda ()
                   (let* ((bf (buffer-name))
                          (mode
                           (catch 'md
                             (mapcar
                              (lambda (x)
                                (and
                                 (string-match (car x) bf)
                                 (throw 'md (symbol-name (cdr x)))))
                              auto-mode-alist))))
                     ;; (setq mode
                     ;;       (or mode
                     ;;           (and (string-equal "*" (substring bf 0 1))
                     ;;                (substring bf 1 -1))))
                     (load
                      (gethash mode *auto-hook-hash*
                               (make-temp-name ""))
                      t))))
      ;; byte-compile
      (when nil
        ;; delete elc without el
        (mapc (lambda(f)(or (file-exists-p (substring f 0 -1))
                        (delete-file f)))
              (directory-files *init-dir* t "\\.elc\\'"))
        ;; recompile
        (eval-when-compile (require 'bytecomp))
        (mapc (lambda(f) (byte-recompile-file f nil 0))
              (directory-files *init-dir* t "\\.el\\'")))
      ;; load *.elc || *.el in *init-dir*
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
      ;;;;;;
      (setq *init-time*
            (cons
             (list 'init
                   (apply '+ (mapcar 'cdr *init-time*)))
             *init-time*))
      ;; when init finished, echo some info
      (add-hook
       'emacs-startup-hook
       '(lambda ()
          (mapc
           (lambda(x) (plist-put (car *init-time*) (car x)(cdr x)))
           (list
            (cons 'emacs
                  (- (float-time after-init-time) (float-time before-init-time)))
            (cons 'other
                  (- (float-time) (float-time after-init-time)))))
          (message "load %d init file, spend %g seconds; startup spend %g seconds"
                   (- (length *init-time*) 1)
                   (plist-get (car *init-time*) 'init)
                   (+
                    (plist-get (car *init-time*) 'emacs)
                    (plist-get (car *init-time*) 'other))))))
    ;;;;;;;;
    "success"))
