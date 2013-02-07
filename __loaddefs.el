(defun loaddefs-update (dir &optional ldfs)
  (let* ((ldfs (or ldfs _loaddefs))
         (var (loaddefs-read ldfs))
         )
    (mapcar
     (lambda(x)
       (loaddefs-prase x)
       )
     (directory-files dir t "\\.el\\'"))))

(defun loaddefs-read (file)
  (let (var)
    (with-current-buffer
        (let ((enable-local-variables nil))
          (find-file-noselect file))
      (goto-char (point-min))
      (setq var (read (current-buffer)))
      (kill-buffer (current-buffer)))
    var))

(defun loaddefs-prase (file &optional prefix base)
  (let ((prefix (or prefix "_autoload/"))
        (base (or base '*init-dir*))
        (type-lst '(defun nil defmacro t))
        var)
    (with-current-buffer
        (let ((enable-local-variables nil))
          (find-file-noselect file))
      (goto-char (point-min))
      (while (search-forward-regexp generate-autoload-cookie nil t)
        (let ((x (read (current-buffer))))
          (setq var 
                (cons 
                 (loaddefs-gen 
                  (nth 1 x)
                  (concat prefix file) base
                  "" 
                  (eq 'interactive 
                      (car (if (stringp (nth 3 x))(nth 4 x)(nth 3 x))))
                  (plist-get type-lst (nth 0 x)))
                 var))))
      (kill-buffer (current-buffer)))
    (setq var (cons (current-time) var)
          var (cons file var))))

(defun loaddefs-eval (var)
  (mapcar
   (lambda(x)
     (mapcar
      (lambda(y)
        (eval y))
      (nthcdr 2 x)))
   var))

(defun loaddefs-write (file var)
  (with-temp-file file
    (with-current-buffer 
        (pp var (current-buffer)))))

(defun loaddefs-gen (fn file dir docstring interactive type)
  `(autoload ',fn (expand-file-name ,file ,dir) ,docstring ,interactive ,type))

(defun loaddefs-file-mtime (file)
  (nth 5 (file-attributes file)))
(defun loaddefs-newer-than-p (file time)
  (time-less-p
   (loaddefs-file-mtime file) time))



;;;(nth 0 (car x))
;;;(nth 1 (car x))
