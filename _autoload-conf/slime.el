(add-to-list 'load-path (expand-file-name "slime" exts-dir))
(require 'slime-autoloads)

(let ((prefix (expand-file-name "../.." exec-directory)))
  (setq slime-lisp-implementations
        (append
         (if (eq system-type 'windows-nt)
             (prog1
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
                                  "mit-scheme/lib" prefix)
                                "--edit")
                               :init mit-scheme-init
                               :coding-system utf-8-unix))
               (setenv "SBCL_Home" (expand-file-name "sbcl" prefix)))
           `((sbcl ("sbcl")
                   :coding-system utf-8-unix)
             (Racket ("racket")
                     :coding-system utf-8-unix)))
         `((R ("R" "--no-save" "--max-vsize=4096M")
              :init (lambda (port-filename coding-system) 
                      (format
                       "source('%s', keep.source=TRUE, chdir=TRUE)\nstartSwank('%s')\n"
                       ,(expand-file-name
                         "_extensions_/swankr/swank.R" *init-dir*)
                       port-filename)))
           (ruby ("ruby") :coding-system utf-8-unix
                 :init (lambda (port-filename coding-system)
                         (format
                          "load \"%s\"\nstart_swank(\"%s\")\n"
                          ,(expand-file-name
                            "_extensions_/slime/contrib/swank.rb" *init-dir*)
                          port-filename)))
           (node.js ("node" "-i")
                    :init (lambda (port-filename coding-system)
                            (format
                             "require(\"%s\")\nstart_swank(\"%s\")\n"
                             ,(expand-file-name
                               "_extensions_/swank-js/swank.js" *init-dir*)
                             port-filename)))))))


;; (setq inferior-lisp-program
;;       (expand-file-name "../../sbcl/sbcl.exe" exec-directory))

;增加lisp代码的自动完成功能
(defun lisp-indent-or-complete (&optional arg)
    (interactive "p")
    (if (or (looking-back "^\\s-*") (bolp))
        (call-interactively 'lisp-indent-line)
        (call-interactively 'slime-indent-and-complete-symbol)))

(eval-after-load "lisp-mode"
    '(progn
       (define-key lisp-mode-map (kbd "TAB") 'lisp-indent-or-complete)))

;;按回车键后下一行代码自动缩进
(add-hook 'lisp-mode-hook '(lambda ()
      (local-set-key (kbd "RET") 'newline-and-indent)))
(put 'upcase-region 'disabled nil)
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
   (let* ((impl
           (mapcar
            (lambda(x)(symbol-name (car x)))
            slime-lisp-implementations))
          (def (car impl)))
     (list
      (completing-read
       (format "Run Lisp (default %s):" def)
       impl nil nil nil nil def))))
  (let ((cmd (read cmd)))
    (slime cmd)))
