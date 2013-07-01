(add-to-list 'load-path (expand-file-name "slime" exts-dir))
(require 'slime-autoloads)

(let* ((prefix (expand-file-name "../.." exec-directory))
       (getn (lambda(x &rest rst)`(,(expand-file-name x prefix) ,@rst)))
       (idx (if (eq system-type 'windows-nt) 1 2))
       (arg-lst
        `((ccl :coding-system utf-8-unix)
          (sbcl :coding-system utf-8-unix)
          (mit-scheme :coding-system utf-8-unix
                      :init (lambda (port-filename coding-system)
                              (format "(begin (load-option 'format)
                                     (load %s (->environment '(swank)))(start-swank %s))"
                                      ,(expand-file-name
                                        "_extensions_/slime/contrib/swank-mit-scheme.scm"
                                        iff-source)
                                      port-filename)))
          (R :init (lambda (port-filename coding-system)
                     (format
                      "source('%s', keep.source=TRUE, chdir=TRUE)\nstartSwank('%s')\n"
                      ,(expand-file-name
                        "_extensions_/swankr/swank.R" iff-source)
                      port-filename)))
          (ruby :coding-system utf-8-unix
                :init (lambda (port-filename coding-system)
                        (format
                         "load \"%s\"\nswank.start_swank(\"%s\")\n"
                         ,(expand-file-name
                           "_extensions_/swank.rb" iff-source)
                         port-filename)))
          (node.js :init (lambda (port-filename coding-system)
                           (format
                            "require(\"%s\")\nstart_swank(\"%s\")\n"
                            ,(expand-file-name
                              "_extensions_/swank-js/swank.js" iff-source)
                            port-filename)))))
       (cmd-lst
        `((sbcl ,(funcall getn "sbcl/sbcl.exe"))
          (ccl ,(funcall getn "ccl/wx86cl.exe"))
          (mit-scheme ,(funcall getn "mit-scheme/bin/mit-scheme.exe"
                                "--library"
                                (expand-file-name
                                 "mit-scheme/lib" prefix)))
          (R ,(funcall getn "R/bin/R.exe"  "--no-save" "--max-vsize=4096M")
             ("R" "--no-save" "--max-vsize=4096M"))
          (node.js ("node" "-i") ("node" "-i")))))
  (if (eq system-type 'windows-nt)
      (progn
        (setenv "SBCL_Home" (expand-file-name "sbcl" prefix))
        (setenv "CCL_DEFAULT_DIRECTORY" (expand-file-name "ccl" prefix))))
  (setq slime-lisp-implementations
        (mapcar
         (lambda(x)
           (let ((name (car x)))
             (cons name
                   (cons (or (nth idx (assoc name cmd-lst))
                             (list (symbol-name name)))
                         (cdr x)))))
         arg-lst)))

;; (setq inferior-lisp-program
;;       (expand-file-name "../../sbcl/sbcl.exe" exec-directory))

;; 增加lisp代码的自动完成功能
(defun lisp-indent-or-complete (&optional arg)
  (interactive "p")
  (if (or (looking-back "^\\s-*") (bolp))
      (call-interactively 'lisp-indent-line)
    (call-interactively 'slime-indent-and-complete-symbol)))

(eval-after-load "lisp-mode"
  '(progn
     (define-key lisp-mode-map (kbd "TAB") 'lisp-indent-or-complete)))

(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

;; 按回车键后下一行代码自动缩进
(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "RET") 'newline-and-indent)))
(put 'upcase-region 'disabled nil)
(global-set-key "\C-cs" 'slime-selector)


;; (setq inferior-lisp-program "lisp")
(slime-setup '(slime-fancy
               slime-autodoc
               slime-asdf
               slime-banner
               slime-clipboard
               slime-highlight-edits
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

;;;###autoload
(defun sbcl ()
  (interactive)
  (slime 'sbcl))

;;;###autoload
(defun R ()
  (interactive)
  (slime 'R))
