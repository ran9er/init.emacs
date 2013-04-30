(defvar script&shell-alist
  '(("py"     .  "python")
    ("rb"     .  "ruby")
    ("lua"    .  "lua")
    ("php"    .  "php")
    ("pl"     .  "perl")
    ("sh"     .  "bash")
    ("java"   .  "javac")
    ("hs"     .  "hugs")
    ("js"     .  "node")
    ))

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
    (setq ext-map script&shell-alist)
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
  (let ((progstr (symbol-name prog)))
    (eval
     `(defun ,(concat-symbol prog '-shell)()
        ,(format "make a %s shell" prog)
        (interactive)
        (switch-to-buffer
         (make-comint ,progstr
                      ,(or
                        (cdr (assoc progstr script&shell-alist))
                        progstr)
                      nil "-i"))))))
;; (mapcar 'x-shell '(python lua))

;;;###autoload
(defun lua-shell ()
  "make a lua shell"
  (interactive)
  (switch-to-buffer (make-comint "lua" "lua" nil "-i")))

;;;###autoload
(defun node.js ()
  "make a node.js shell"
  (interactive)
  (switch-to-buffer (make-comint "node.js" "node" nil "-i")))

;;;###autoload
(autoload 'run-ruby "inf-ruby"
"Run an inferior Ruby process" t)
