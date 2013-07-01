;; (unless (boundp 'iff-source)
;;   (defvar iff-source
;;     ((lambda (init-name-match base-dir)
;;        ((lambda (x) (file-name-as-directory
;;                  (or (car (sort x 'file-newer-than-file-p))
;;                      (make-temp-name ""))))
;;         (mapcar (lambda (x) (if (file-directory-p x) x (make-temp-name "")))
;;                 (directory-files base-dir t init-name-match t))))
;;      "init.*emacs\\|emacs.*init"
;;      (apply 'expand-file-name
;;             (cond
;;              ((eq system-type 'windows-nt)
;;               `(".." ,exec-directory))
;;              (t
;;               `("~"))))))
;;   (load
;;    (expand-file-name
;;     "bootstrap.el"
;;     iff-source)))

(load "../iff/__bootstrap")
