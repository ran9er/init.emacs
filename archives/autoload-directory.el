;; -*- encoding: utf-8-unix; -*-
;; File-name:    <autoload-directory.el>
;; Create:       <2011-12-31 23:56:10 ran9er>
;; Time-stamp:   <2013-02-10 01:53:14 ran9er>
;; Mail:         <2999am@gmail.com>


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
                  (while (setq i (search-forward-regexp "^;;; Generated autoloads from " p t))
                    (setq out (cons (buffer-substring-no-properties i (line-end-position))
                                    out)))) out))
            ldfs))
     (setq force (or f (null (equal (reverse index)
                                    (directory-files path nil "\\.el\\'")))))
     (if force (delete-file ldfs))
     (let ((generated-autoload-file ldfs))
       (mapcar
        (lambda (fl)
          (if (or force
                  (null (file-newer-than-file-p generated-autoload-file fl)))
              ;; if (>= emacs-major-version 24)
              ;; (update-file-autoloads fl t ldfs)
              (update-file-autoloads fl t)))
        (directory-files path t "\\.el\\'")))
     (load ldfs)))
 "_autoload_/")

(funcall
 (lambda (dir &optional loaddefs basedir)
   (let* ((path
           (expand-file-name dir (or basedir *init-dir*)))
          (generated-autoload-file
           (expand-file-name (or loaddefs "_loaddefs") path)))
     (update-directory-autoloads path)
     (load generated-autoload-file)))
 "_autoload_/")

(_autoload
 (lambda (dir &optional loaddefs basedir)
   (let* ((path
           (expand-file-name dir (or basedir *init-dir*)))
          (ldfs
           (or loaddefs atld-df))
          (generated-autoload-file
           (expand-file-name ldfs path)))
     (~ _check-directory path t basedir)
     (update-directory-autoloads path)
     (kill-buffer ldfs)
     (load generated-autoload-file))))
