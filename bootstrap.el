(defvar *init-status*
  ;;;;;;;;;;
  (catch 'quit
    ;; to avoid loading this file recursively
    (if (boundp '*init-time*)
        (throw 'quit "have been loaded"))
    ;;;;;;;;
    (let ((this-file (file-name-nondirectory load-file-name))
          (this-dir (file-name-directory load-file-name))
          (tmp (make-temp-name ""))
          init-name-match base-dir init-dir init-files)
      ;;;;;;
      (cond
       ; when specify *init-dir* outside, then load this file
       ((boundp '*init-dir*) nil)
       ; when this file's name is .emacs or site-start.el
       ((member load-file-name
                (mapcar 'expand-file-name
                        (list "~/.emacs"
                              (or (locate-library site-run-file) tmp))))
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
                   ; *init-dir* is the newest directory with "init" and "emacs" in name
                   ; or directory where this file is located
                   (or (car (sort x 'file-newer-than-file-p)) this-dir)))
          (mapcar (lambda (x) (if (file-directory-p x) x tmp))
                  (directory-files base-dir t init-name-match t)))))
       ; when this file's name is not .emacs or site-start.el, for example as bootstrap.el
       ; load this file in emacs init file : (load "...../bootstrap.el")
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
      ;; *feature-file-hash*
      (defvar *feature-file-hash* (make-hash-table :test 'equal :size 20))
      (mapc
       (lambda (x)
         (puthash
          (intern (file-name-sans-extension (file-name-nondirectory x))) x
          *feature-file-hash*))
       (directory-files (expand-file-name "_extensions/" *init-dir*) t "\\.el\\'"))
      ;;;;;;
      (maphash
       (lambda (x y)
         (eval-after-load x `(load ,y)))
       *feature-file-hash*)
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
