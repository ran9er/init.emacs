;; -*- encoding: utf-8-unix; -*-
;; File-name:    <+script.el>
;; Create:       <2012-02-14 00:21:56 ran9er>
;; Time-stamp:   <2012-02-15 00:57:56 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl,
then it'll call “perl x.pl” in a shell.
The file can be php, perl, python, bash, java.
File suffix is used to determine what program to run."
  (interactive)
  (let (ext-map file-name file-ext prog-name cmd-str
                outputf status)
    (setq ext-map
          (to-alist
           '(
             "py"       "python"
             "rb"       "ruby"
             "lua"      "lua"
             "php"      "php"
             "pl"       "perl"
             "sh"       "bash"
             "java"     "javac"
             "hs"       "hugs"
             )))
    (setq
     status
     (catch 'status
       (or (setq file-name (buffer-file-name))
           (throw 'status "isn't a file!"))
       (or (setq file-ext (file-name-extension file-name))
           (throw 'status "no file ext name!"))
       (or (setq prog-name (cdr (assoc file-ext ext-map)))
           (throw 'status "how to do?"))
       (setq cmd-str (concat prog-name " " file-name))
       (setq outputf (if (equal (this-command-keys) [f5]) 'compile 'shell-command))
       (and (funcall outputf cmd-str)
            (throw 'status nil))))
    (and status (message "%s" status))))

;;;###autoload
(defun x-shell (prog)
  (eval
   `(defun ,(concat-symbol prog '-shell)()
      ,(format "make a %s shell" prog)
      (interactive)
      (switch-to-buffer (make-comint ,(symbol-name prog) ,(symbol-name prog) nil "-i")))))
;; (mapcar 'x-shell '(python lua))

;; (defmacro x-shell (prog)
;;   `(defun ,(concat-symbol prog '-shell)()
;;      ,(format "make a %s shell" prog)
;;      (interactive)
;;      (switch-to-buffer (make-comint ,(symbol-name prog) ,(symbol-name prog) nil "-i"))))
;; (mapcar (lambda(x)(eval `(x-shell ,x))) '(python lua))

;;;###autoload
(defun lua-shell ()
  "make a lua shell"
  (interactive)
  (switch-to-buffer (make-comint "lua" "lua" nil "-i")))

;;;###autoload
(autoload 'run-ruby "inf-ruby"
"Run an inferior Ruby process" t)
