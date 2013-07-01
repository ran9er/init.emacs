(defvar initial-framework-for-emacs nil)

(defvar iff--message
  (lambda (msg)
    (message (concat "=======>" msg))))

(defvar iff-log nil)

(defvar iff--add-log
  (lambda (msg &optional head log)
    (let ((log (or log 'iff-log)))
      (set log
           (if head
               (cons msg (eval log))
             (append (eval log)(list msg)))))))

(defvar iff--find-source
  (lambda (regexp base)
    (let* ((tm (float-time))
           (this-file (file-name-nondirectory load-file-name))
           (this-dir (file-name-directory load-file-name)))
      (funcall iff--message "Find source...")
      (prog1
          (cond
           ;; when specify iff-source outside, and load this file
           ((boundp 'iff-source) nil)
           ;; when this file's name is /home/.../.emacs or /.../emacs/.../site-start.el
           ((member load-file-name
                    (mapcar 'expand-file-name
                            (list "~/.emacs" (locate-library site-run-file))))
            ((lambda (x) (file-name-as-directory
                     ;; iff-source is the newest directory with regexp in it's name
                     ;; or directory where this file is located (iff-source is $HOME or site-lisp)
                     (or (car x) this-dir)))
             (setq iff-source-candidates
                   (funcall iff--choice-files
                            regexp base 'file-newer-than-file-p 'file-directory-p))))
           ;; when this file's name is not .emacs or site-start.el, for example as bootstrap.el
           ;; load this file in emacs init file : (load "...../bootstrap.el")
           (t this-dir))
        (funcall iff--add-log
                 (cons "find source"
                       (- (float-time) tm)))))))

(defvar iff--choice-files
  (lambda (regexp base &optional srt flt)
    (let ((flt (or flt 'identity))
          result)
      (mapc
       (lambda (f) (if (funcall flt f)(setq result (append result (list f)))))
       (directory-files base t regexp srt))
      result)))

(defvar iff--find-files
  (lambda (dir exp)
    (mapcar
     (lambda (f) (file-name-sans-extension f))
     (directory-files dir t exp))))

(defvar iff-source-candidates nil)

(defvar iff-source
  (funcall iff--find-source
           "iff\\|init.*el\\|init.*emacs\\|emacs.*init"
           (apply 'expand-file-name
                  (cond
                   ((eq system-type 'windows-nt)
                    `(".." ,exec-directory))
                   (t
                    `("~"))))))

(defvar iff-branch
  (mapcar
   (lambda(x)
     (cons
      (car x)
      (expand-file-name (cdr x) iff-source)))
   '((lib-dir   .   "_lib/")
     (lib-df    .   "_loaddefs")
     (ext-dir   .   "_extensions_")
     (eal-dir   .   "_eval-after-load/")
     (alc-dir   .   "_autoload-conf/")
     (wk-dir    .   "sandbox/"))))

(defvar iff-pre-init-files
  (funcall iff--find-files iff-source "^__.*\\.el\\'"))
(defvar iff-init-files
  (funcall iff--find-files iff-source "^[^_].*\\.el\\'"))

(defvar iff--check-directory
  (lambda (p base &optional dir-p)
    (let ((f (expand-file-name p base)))
      (unless (file-exists-p f)
        (if dir-p
            (progn (make-directory f)
                   (message (concat "New dir " f)))
          (progn (find-file f)
                 (message (concat "New file " f))))))))

(defvar iff--add-load-path
  (lambda(regexp base)
    (funcall iff--message "Add load-path")
    (mapc
     (lambda (p)
       (if (file-directory-p p)
           (and
            (add-to-list 'load-path p)
            (message (format "Add-to-load-path %s" p)))))
     (directory-files base t regexp))))

(defvar iff--load
  (lambda (lst)
    (let* (tm)
      (funcall iff--message (format "Load %s" lst))
      (mapc
       (lambda (f)
         (setq tm (float-time))
         (load f)
         (funcall iff--add-log
                  (cons (file-name-nondirectory f)
                        (- (float-time) tm))))
       (eval lst)))))

(defvar iff--autoload
  (lambda (path)
    (let* ((dir (cdr (assoc path iff-branch)))
           (load-file-name (expand-file-name (make-temp-name "") dir)))
      (funcall iff--message (format "Auto-load %s" path))
      (funcall iff--check-directory dir iff-source t)
      (funcall iff--add-log
               (cons (format "autoload for %s" path)
                     (lazily dir))))))

(defvar iff--eval-after-load
  (lambda (path)
    (let ((tm (float-time))
          (dir (cdr (assoc path iff-branch))))
      (funcall iff--message (format "Eval-after-load %s" path))
      (funcall iff--check-directory dir iff-source t)
      (mapc
       (lambda(x)
         (eval-after-load
             (intern (file-name-sans-extension (file-name-nondirectory x)))
           `(load ,x))
         (message (format "eval-after-load %s" x)))
       (directory-files dir t "\\.el\\'"))
      (funcall iff--add-log
               (cons "gen eval-after-load"
                     (- (float-time) tm))))))

(defvar iff--startup-hook
  '(lambda ()
     (mapc
      (lambda(x) (plist-put (car iff-log) (car x)(cdr x)))
      (list
       (cons 'emacs
             (- (float-time after-init-time) (float-time before-init-time)))
       (cons 'other
             (- (float-time) (float-time after-init-time)))))
     (message "Through %d steps, took %g seconds; startup took %g seconds"
              (- (length iff-log) 1)
              (plist-get (car iff-log) 'init)
              (+
               (plist-get (car iff-log) 'emacs)
               (plist-get (car iff-log) 'other)))))

(defvar iff--init
  (lambda (name &optional value)
    (setq value (eval name))

    (set 'debug-on-error (null initial-framework-for-emacs))

    (if (file-exists-p value)
        (message (format "%s is %s" name value))
      (throw 'quit (format "Can't found %s in %s." name value)))

    (if initial-framework-for-emacs
        (throw 'quit "Have been loaded.")
      (setq initial-framework-for-emacs t))

    (funcall iff--load 'iff-pre-init-files)
    (funcall iff--add-load-path "^_.*_\\'" value)
    (funcall iff--autoload 'lib-dir)
    (funcall iff--autoload 'alc-dir)
    (funcall iff--eval-after-load 'eal-dir)
    (funcall iff--load 'iff-init-files)

    (funcall iff--add-log
             (list 'init (apply '+ (mapcar 'cdr iff-log)))
             t)

    (add-hook 'emacs-startup-hook iff--startup-hook)

    (set 'debug-on-error (null initial-framework-for-emacs))

    "success"))

(defvar iff-status
  (catch 'quit (funcall iff--init 'iff-source)))
