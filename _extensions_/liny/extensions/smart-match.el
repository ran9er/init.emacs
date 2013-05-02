;; * match
(setq liny-match-strategy 'liny-smart-match)

;; * index
(defun liny-intersection (a b)
  "liny-intersection is writen by ran9er"
  (let (result)
    (while (and a b)
      (cond
       ((equal (car a)(car b))
        (setq result (append result (list (car a)))
              a (cdr a)
              b (cdr b)))
       ((string-lessp (car a)(car b))
        (setq a (cdr a)))
       (t
        (setq b (cdr b)))))
    result))

(defun liny-union-set (a b &optional sort-p)
  "liny-union-set is writen by ran9er"
  (let ((lst (delete-dups (append a b))))
    (if sort-p
        (sort lst 'string-lessp)
      lst)))

(defun liny-intersection-r (a b &optional result)
  "liny-intersection-r is writen by ran9er"
  (if (and a b)
      (cond
       ((equal (car a)(car b))
        (liny-intersection-r (cdr a) (cdr b) (cons (car a) result)))
       ((string-lessp (car a)(car b))
        (liny-intersection-r (cdr a) b result))
       (t
        (liny-intersection-r a (cdr b) result)))
    result))

(defun liny-hash-push (key file hash)
  (let ((v (gethash key hash)))
    (if (null (member file v))
        (puthash key (sort (append (list file) v) 'string-lessp) hash))))


(defun liny-gen-index-k ()
  (let ((gs
         (lambda(x)
           (sort
            (remove "" (if x (split-string x "[ \t\n]")))
            'string-lessp)))
        (alias (make-hash-table :test 'equal))
        (modes (make-hash-table :test 'equal))
        (files (make-hash-table :test 'equal)))
    (mapc
     (lambda(x)
       (with-temp-buffer
         (let (necessary sufficient)
           (insert-file-contents (expand-file-name x liny-repo))
           (mapc (lambda(y)(liny-hash-push y x alias))
                 (funcall gs (liny-search-str "alias")))
           (mapc (lambda(y)(liny-hash-push y x modes))
                 (funcall gs (liny-search-str "modes")))
           (mapc
            (lambda(x)
              (if (equal "+" (substring x 0 1))
                  (setq necessary (cons (substring x 1) necessary))
                (setq sufficient (cons x sufficient))))
            (funcall gs (liny-search-str "keywords")))
           (puthash
            x
            (list
             necessary
             sufficient)
            files))))
     (directory-files liny-repo nil "^[^._].*\\'"))
    (list files alias modes)))

(defun liny-snippet-exist-p (snippet)
  (gethash snippet liny-files-index))

(defun liny-update-keyword-index (file strs &optional force)
  "liny-update-keyword-dir is writen by ran9er"
  (let* ((lst (liny-read-index (liny-update-index-dir file strs force))))
    (setq liny-files-index (nth 0 lst)
          liny-alias-index (nth 1 lst)
          liny-modes-index (nth 2 lst))))

(liny-update-keyword-index "_keywords_index" '(liny-gen-index-k))

(defun liny-force-update-keyword ()
  (interactive)
  (liny-update-keyword-index "_keywords_index" '(liny-gen-index-k) t)
  (liny-clear-cache))

;; (insert (concat "\n" (pp-to-string (liny-gen-index-k))))

;; * match
(defvar liny-env-test
  #s(hash-table size 40 test equal rehash-size 1.5 rehash-threshold 0.8 data
                ("head" ((progn (funcall liny-fetch-alias-func)
                               (skip-chars-backward " \t\n")(bobp)))
                 "tail" ((progn (skip-chars-forward " \t\n")(eobp)))
                 "notop" ((null (zerop (current-indentation))))
                 "top" ((zerop (current-indentation)))
                 "hol" ((progn (funcall liny-fetch-alias-func)
                               (skip-chars-backward " \t")(bolp)))
                 "nohol"((null (progn (funcall liny-fetch-alias-func)
                               (skip-chars-backward " \t")(bolp))))
                 )))


(defun liny-exec-env-test (keyword)
  (let ((env (if (hash-table-p liny-env-test)
                 (gethash keyword liny-env-test)
               (cdr (assoc keyword liny-env-test))))
        (test
         (lambda(tst)
           (if (save-excursion (save-restriction (eval (car tst))))
               (cons keyword (or (nth 1 tst) 1))))))
    (funcall test env)))

(defun liny-keywords-match (&optional necessary sufficient)
  "liny-keywords-match is writen by ran9er"
  (let* ((result 0))
    (catch 'test
      (while (and
              necessary
              (if (liny-exec-env-test (car necessary))
                  t
                (throw 'test nil)))
        (setq necessary (cdr necessary)))
      (mapc
       (lambda(x)
         (let ((envt (liny-exec-env-test x)))
           (if envt (setq result (+ (cdr envt) result)))))
       sufficient)
      result)))

(defun liny-fetch-env-mode ()
  "liny-fetch-env-mode is writen by ran9er"
  (symbol-name major-mode))

;; *
(defun liny-smart-match ()
  (let* ((alias (gethash (liny-fetch-alias) liny-alias-index))
         (modes (gethash (liny-fetch-env-mode) liny-modes-index))
         (files (liny-union-set
                 (liny-intersection alias modes)
                 (liny-intersection alias (gethash "all" liny-modes-index))))
         (result (sort
                  (mapcar
                   (lambda(x)
                     (let* ((lst (gethash x liny-files-index))
                            (match (if (or (nth 0 lst)(nth 1 lst))
                                       (apply 'liny-keywords-match lst)
                                     0)))
                       (if match (cons match x)
                         (cons -1 x))))
                   files)
                  (lambda(x y)
                    (> (car x)(car y))))))
    (if (and
         result
         (or
          (boundp 'liny-match-snippets)
          (make-local-variable 'liny-match-snippets)))
        (setq liny-match-snippets result))
    (if (and result (>= (caar result) 0)) (cdar result))))
