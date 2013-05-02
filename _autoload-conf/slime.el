(add-to-list 'load-path (expand-file-name "slime" exts-dir))
(require 'slime-autoloads)

(if (eq system-type 'windows-nt)
    (progn
      (setq slime-lisp-implementations
            `((ccl (,(expand-file-name "../../ccl/wx86cl.exe" exec-directory))
                   :coding-system utf-8-unix)
              (sbcl (,(expand-file-name "../../sbcl/sbcl.exe" exec-directory))
                    :coding-system utf-8-unix)
              (Racket (,(expand-file-name "../../Racket/Racket.exe" exec-directory))
                      :coding-system utf-8-unix)
              (cmucl ("cmucl" "-quiet"))))
      (setenv "SBCL_Home" (expand-file-name "../../sbcl" exec-directory))))

;; (setq inferior-lisp-program
;;       (expand-file-name "../../sbcl/sbcl.exe" exec-directory))

(global-set-key "\C-cs" 'slime-selector)
(setq inferior-lisp-program "lisp")
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
