(add-to-list 'load-path (expand-file-name "slime" exts-dir))
(require 'slime-autoloads)

(let ((prefix (expand-file-name "../.." exec-directory)))
  (if (eq system-type 'windows-nt)
      (progn
        (setq slime-lisp-implementations
              `((ccl (,(expand-file-name "ccl/wx86cl.exe" prefix))
                     :coding-system utf-8-unix)
                (sbcl (,(expand-file-name "sbcl/sbcl.exe" prefix))
                      :coding-system utf-8-unix)
                (Racket (,(expand-file-name "Racket/Racket.exe" prefix))
                        :coding-system utf-8-unix)
                (mit-scheme (,(expand-file-name
                               "mit-scheme/bin/mit-scheme.exe" prefix)
                             "--library"
                             ,(expand-file-name
                               "mit-scheme/lib" prefix))
                            :init mit-scheme-init)
                (cmucl ("cmucl" "-quiet"))))
        (setenv "SBCL_Home" (expand-file-name "sbcl" prefix)))))

;; (setq inferior-lisp-program
;;       (expand-file-name "../../sbcl/sbcl.exe" exec-directory))

(global-set-key "\C-cs" 'slime-selector)
;(setq inferior-lisp-program "lisp")
(slime-setup '(slime-fancy
               slime-asdf
               slime-banner
               slime-clipboard
               slime-scheme
               ))
(setq slime-complete-symbol*-fancy t)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

;;;###autoload
(autoload 'slime "slime-autoloads" "" t)

;;;###autoload
(defun sli (cmd)
  (interactive
   (let ((impl
          (mapcar
           (lambda(x)(symbol-name (car x)))
           slime-lisp-implementations)))
     (list
      (completing-read
       (concat "Run Lisp (default ccl):")
       impl nil nil nil nil (car impl)))))
  (let ((cmd (read cmd)))
    (slime cmd)))
