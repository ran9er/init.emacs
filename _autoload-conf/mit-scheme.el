(defun mit-scheme-init (file encoding)
  (format "%S\n\n"
      `(begin
        (load-option 'format)
        (load-option 'sos)
        (eval
         '(construct-normal-package-from-description
           (make-package-description '(swank) '(())
                     (vector) (vector) (vector) false))
         (->environment '(package)))
        (load ,(expand-file-name
                "_extensions_/slime/contrib/swank-mit-scheme.scm" *init-dir*)
          (->environment '(swank)))
        (eval '(start-swank ,file) (->environment '(swank))))))

;;;###autoload
(defun mit-scheme ()
  (interactive)
  (slime 'mit-scheme))

(defun find-mit-scheme-package ()
  (save-excursion
    (let ((case-fold-search t))
      (and (re-search-backward "^[;]+ package: \\((.+)\\).*$" nil t)
       (match-string-no-properties 1)))))

(setq slime-find-buffer-package-function 'find-mit-scheme-package)
(add-hook 'scheme-mode-hook (lambda () (slime-mode 1)))
