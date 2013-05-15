;; -*- encoding: utf-8-unix; -*-
;; File-name:    <auto-hook-hash.el>
;; Create:       <2012-03-05 00:17:52 ran9er>
;; Time-stamp:   <2012-03-05 00:18:19 ran9er>
;; Mail:         <2999am@gmail.com>
      ;; *auto-hook-hash*
      (defvar *auto-hook-hash* (make-hash-table :test 'equal :size 20))
      (mapc
       (lambda (x)
         (puthash
          (intern (file-name-sans-extension (file-name-nondirectory x))) x
          *auto-hook-hash*))
       (directory-files (expand-file-name "_extensions/" *init-dir*) t "\\.el\\'"))
      ;;;;;;1
      (maphash
       (lambda (x y)
         (eval-after-load x `(load ,y)))
       *auto-hook-hash*)
      ;;;;;;2
      (add-hook 'find-file-hook
                `(lambda ()
                   (let* ((bf (buffer-name))
                          (mode
                           (catch 'md
                             (mapcar
                              (lambda (x)
                                (and
                                 (string-match (car x) bf)
                                 (throw 'md (cdr x))))
                              auto-mode-alist))))
                     ;; (setq mode
                     ;;       (or mode
                     ;;           (and (string-equal "*" (substring bf 0 1))
                     ;;                (substring bf 1 -1))))
                     (load
                      (gethash mode *auto-hook-hash* ,tmp)
                      t))))
