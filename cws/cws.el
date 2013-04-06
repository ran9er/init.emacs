(defvar cws-path (file-name-directory
                  (or load-file-name (buffer-file-name))))

(defvar cws-words-file (expand-file-name "words.txt" cws-path))
(defvar cws-words-index (expand-file-name "words.idx" cws-path))

(defun cws-update-index-dir (file strs base &optional force)
  (let ((mtime (lambda(x)(nth 5 (file-attributes x))))
        print-length print-level selective-display-ellipses)
    (if (or force
            (time-less-p
             (or (funcall mtime file)
                 '(0 0 0))
             (funcall mtime base)))
        (with-temp-file
            (let ((enable-local-variables nil)
                  (find-file-hook nil))
              file)
          (insert (format "%S" (eval strs)))
          (message (format "Save %s." file))))
    file))

(defun cws-set (lst k &optional v)
  "cws-set is writen by ran9er"
  (let ((e (assoc k lst)))
    (if e
        (setcdr e (list v))
      (setcdr (last lst)(list (if v (list k v)(list k)))))))

(defun cws-make-tree (lst &optional reverse)
  "cws-make-tree is writen by ran9er"
  (let* ((len (length lst))
         (total (float len))
         (result '(nil))
         (ins (lambda(s ls)
                (let ((l (length s)))
                  (if (> l 0)
                      (let* ((a (if reverse
                                    (substring s (1- l))
                                  (substring s 0 1)))
                             (d (if reverse
                                    (substring s 0 (1- l))
                                  (substring s 1)))
                             (r (assoc a ls)))
                        (if r
                            (funcall ins d (cdr r))
                          (cws-set ls a)
                          (funcall ins d (assoc a ls)))))))))
    (while (> len 0)
      (let ((w (aref lst (1- len))))
        (funcall ins w result))
      (setq len (1- len))
      (message (format "%d, %d/100" len (- 100 (* 100 (/ len total))))))
    (remove nil result)))

(defun cws-update-index()
  (interactive)
  (cws-update-index-dir
   cws-words-index
   '(cws-make-tree (funcall (plist-get cws-cl 'words)))
   cws-words-file))

(defvar
  cws-cl
  (funcall
   (eval
    '(lambda(cws-path)
       (mapc
        (lambda(x)(modify-syntax-entry (car x)(cadr x)))
        '((?_ "w")(?[ "w")(?] "w")(?（ "(")(?） ")")
          (?， ".")(?。 ".")(?“ "(")(?” ")")(?… ".")))
       (let* (index cc lz (n 0)
              (word? (lambda()(if (null cc)
                          (setq
                           cc
                           (eval lz))
                        cc)))
              (max? (lambda(lst)
                      (mapc (lambda(x)(if (> (length x) n)(setq n (length x)))) lst))))
         (setq lz (quote
                   `[,@(split-string
                        (with-current-buffer
                            (let ((enable-local-variables nil))
                              (find-file-noselect cws-words-file))
                          (prog1
                              (condition-case err
                                  (buffer-substring (point-min) (point-max))
                                (error
                                 nil))
                            (kill-buffer (current-buffer)))) "\n")]))
         (with-current-buffer
             (let ((enable-local-variables nil))
               (find-file-noselect cws-words-index))
           (prog2
               (goto-char (point-min))
               (setq index
                     (condition-case err
                         (read (current-buffer))
                       (error
                        nil)))
             (kill-buffer (current-buffer))))
         (list
          'index (lambda() index)
          'skip (lambda() "…_[]（），。“” \t\n")
          'words word?
          'max (lambda() (if (null (zerop n)) n
                       (funcall max? (funcall (plist-get cws-cl 'words)))
                       n)))))
    t)
   (file-name-directory
    (or load-file-name (buffer-file-name)))))

(defun cws-search-word (word lst)
  (let ((r 0))
    (while (and
            (> (length word) 0)
            (setq lst (assoc (substring word 0 1) lst)))
      (setq word (substring word 1))
      (setq r (1+ r)))
    r))

(defun cws-search-word-1 (word lst)
  "cws-search-word is writen by ran9er"
  (let* ((beg 0)
         (end (length lst))
         (cnt 0)
         mid result)
    (while (progn
             (setq mid (/ (+ beg end) 2)
                   mwd (aref lst mid))
             (cond
              ((string-equal word mwd)
               (progn (setq result word) nil))
              ((>= (1+ beg) end) nil)
              (t t)))
      (if (string-lessp word mwd)
          (setq end mid)
        (setq beg mid))
      (setq cnt (1+ cnt)))
    (cons result cnt)))


(defun cws-get-str (&optional back)
  "cws-get-word is writen by ran9er"
  (let ((mov (if back "backward" "forward")))
    (buffer-substring-no-properties
     (progn
       (funcall (intern (concat "skip-chars-" mov))
                (funcall (plist-get cws-cl 'skip)))
       (point))
     (save-restriction
       (save-excursion
         (funcall (intern (concat mov "-word")))
         (point))))))

(defun cws-forward-word ()
  (interactive)
  (let ((r (cws-search-word (cws-get-str) (funcall (plist-get cws-cl 'index)))))
    (if (> r 0)
        (forward-char r)
      (forward-word))))

(defun cws-backward-word ()
  (interactive)
  (let* ((r (cws-get-str t))
         (l (length r))
         (mx 10)
         m)
    (if (> l mx) (setq r (substring r (- l mx))))
    (while (and (> (length r) 0)
                (> (setq m (cws-search-word r (funcall (plist-get cws-cl 'index)))) 0))
      (setq r (substring r m)))
    (if (> m 0)
        (backward-char m)
      (backward-word))))

(define-minor-mode cws-mode
  "Buffer-local minor mode to move word by chinese word."
  :group 'cws
  :global t
  :lighter " CWS"
  :keymap
  `((,(kbd "M-f") . cws-forward-word)
    (,(kbd "M-b") . cws-backward-word))
  )
