(defun compare-sets-s (a b)
  (let* ((c 0)
         (l (mapcar (lambda(x)(and (member x a) t)) b))
         (n a)
         m z)
    (setq
     z
     (remove
      nil
      (mapcar
       (lambda(y)
         (prog1
             (if y
                 (nth c b)
               (setq m (cons (nth c b) m))
               nil)
           (setq c (1+ c))))
       l)))
    (mapcar (lambda(x)(setq n (remove x n))) z)
    (list z n m)))

(defun loaddefs-eval (var)
  (mapcar
   (lambda(x)
     (mapcar
      (lambda(y)
        (eval y))
      (nthcdr 2 x)))
   var))

(defun loaddefs-file-mtime (file)
  (nth 5 (file-attributes file)))
(defun loaddefs-newer-than-p (file time)
  (time-less-p
   (loaddefs-file-mtime file) time))

(defun loaddefs-read (ldfs)
  (let (var)
    (with-current-buffer
        (let ((enable-local-variables nil))
          (find-file-noselect ldfs))
      (goto-char (point-min))
      (setq var
            (condition-case err
                (read (current-buffer))
              (error
               nil)))
      (kill-buffer (current-buffer)))
    var))

(defun loaddefs-save (ldfs var)
  (with-temp-file ldfs
    (with-current-buffer
        (let ((enable-local-variables nil))
          (find-file-noselect ldfs))
      (insert (pp-to-string var))
      (message (concat "Save " ldfs))
      (kill-buffer (current-buffer)))))

(defun loaddefs-gen (fn file dir docstring interactive type)
  `(autoload ',fn
     (expand-file-name ,file ,dir)
     ,docstring
     ,interactive
     ,type))

(defun loaddefs-docstr (s a)
  (let* ((a (mapconcat 'symbol-name a " "))
         (a (if (> (length a) 0)(concat " " a) a)))
    (format "\
%s

\(fn%s)" s a)))

(defun loaddefs-prase (file dir base)
  (let ((f (expand-file-name file dir))
        (type-lst '(defun nil defmacro t))
        var)
    (with-current-buffer
        (let ((enable-local-variables nil))
          (find-file-noselect f))
      (goto-char (point-min))
      (while (search-forward-regexp ";;;###autoload" nil t)
        (let ((x (read (current-buffer))))
          (if (memq (car x) type-lst)
              (setq var
                    (cons
                     (eval
                      `(loaddefs-gen
                        (nth 1 x)
                        (file-name-sans-extension
                         (file-relative-name f (eval base)))
                        ',base
                        (loaddefs-docstr
                         (if (stringp (nth 3 x))(nth 3 x)"Not documented.")
                         (nth 2 x))
                        (eq 'interactive
                            (car (if (stringp (nth 3 x))(nth 4 x)(nth 3 x))))
                        (plist-get type-lst (nth 0 x))))
                     var)))))
      (kill-buffer (current-buffer)))
    (setq ;; var (remove nil var)
     var (cons (current-time) (reverse var))
     var (cons file var))))

(defun loaddefs-update (dir &optional base ldfs)
  (let* ((base (or base '*init-dir*))
         (ldfs (or ldfs "_loaddefs"))
         (lf (expand-file-name ldfs dir))
         (ld (expand-file-name dir))
         (var (loaddefs-read lf))
         (ld-files (mapcar (lambda(x)(car x)) var))
         (files (directory-files ld nil "\\.el\\'"))
         (df (compare-sets-s ld-files files))
         (d0 (car df))(d1 (nth 1 df))(d2 (nth 2 df)))
    (mapc (lambda(x)(setq var (remove (assoc x var) var))) d1)
    (mapcar
     (lambda(x)(setq var
                 (cons
                  ;; (loaddefs-prase x dir)
                  var))) d0)
    (mapc (lambda(x)(setq var (cons (loaddefs-prase x ld base) var))) d2)
    (setq var (remove nil var))
    (loaddefs-save lf var)))

;(test-times 1       (loaddefs-update (expand-file-name "_autoload" *init-dir*) '*init-dir*))