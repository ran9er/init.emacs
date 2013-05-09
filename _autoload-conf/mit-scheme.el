(when (require 'slime nil t)

;;;###autoload
  (defun mit-scheme-start-swank (file encoding)
    (format "%S\n\n" `(start-swank ,file)))

  (defun mit-scheme-find-buffer-package ()
    (save-excursion
      (let ((case-fold-search t))
	(goto-char (point-min))
	(and (re-search-forward "^;+ package: \\(([^)]+)\\)" nil t)
	     (match-string-no-properties 1)))))

  (defun mit-scheme-slime-mode-init ()
    (slime-mode t)
    (make-local-variable 'slime-find-buffer-package-function)
    (setq slime-find-buffer-package-function 'mit-scheme-find-buffer-package))

  (slime-setup)
  (if (not (assq 'mit-scheme slime-lisp-implementations))
      (setq slime-lisp-implementations
	    (cons '(mit-scheme ("mit-scheme")
			       :init mit-scheme-start-swank)
		  slime-lisp-implementations)))
  (setq slime-default-lisp 'mit-scheme)
  (add-hook 'scheme-mode-hook 'mit-scheme-slime-mode-init))

;;;###autoload
(defun mit-scheme-init (file encoding)
  (format "%S\n\n"
      `(begin
        (load-option 'format)
        ;; (load-option 'sos)
        ;; (eval
        ;;  '(construct-normal-package-from-description
        ;;    (make-package-description '(swank) '(())
        ;;              (vector) (vector) (vector) false))
        ;;  (->environment '(package)))
        (load ,(expand-file-name
                "_extensions_/slime/contrib/swank-mit-scheme.scm" *init-dir*)
          (->environment '(runtime swank)))
        ;; (eval '(start-swank ,file) (->environment '(swank)))
        (start-swank ,file))))

;;;###autoload
(defun mit-scheme ()
  (interactive)
  (slime 'mit-scheme))
