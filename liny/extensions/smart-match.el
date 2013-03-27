;; * match
(setq liny-match-strategy 'liny-smart-match)

(defvar liny-alias-index
  (make-hash-table :test 'equal))

(defvar liny-files-index
  (make-hash-table :test 'equal))

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

(defun liny-union-set (a b)
  "liny-union-set is writen by ran9er"
  (delete-dups (sort (append a b) 'string-lessp)))

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
  '(("head" (progn (funcall liny-fetch-alias-func)
                   (skip-chars-backward " \t\n")(bobp)))
    ("tail" (progn (skip-chars-forward " \t\n")(eobp)))
    ("notop" (null (zerop (current-indentation))))
    ("top" (zerop (current-indentation)))))

(defun liny-fetch-env ()
  "liny-fetch-env "
  (let ((test
         (lambda(tst)
           (sort
            (remove
             nil
             (mapcar
              (lambda(x)
                (if (save-excursion (save-restriction (eval (nth 1 x))))
                    (cons (nth 0 x) (or (nth 2 x) 1))))
              tst))
            (lambda(x y)
              (string-lessp (car x)(car y)))))))
        (funcall test liny-env-test)))

(defun liny-fetch-env-mode ()
  "liny-fetch-env-mode is writen by ran9er"
  (symbol-name major-mode))

(defun liny-keywords-match (&optional necessary sufficient)
  "liny-keywords-match is writen by ran9er"
  (let* ((env (liny-fetch-env))
         (result 0)
         envl)
    (and
     (catch 'test
       (setq envl (mapcar (lambda(x)(car x)) env))
       (while (and
               necessary
               (if (member (car necessary) envl) t
                 (throw 'test nil)))
         (setq necessary (cdr necessary)))
       (mapc
        (lambda(x)
          (if (member (car x) sufficient)
              (setq result (+ (cdr x) result))))
        env))
     result)))

(defvar liny-match-files nil)

;; *
(defun liny-smart-match ()
  (let* ((alias (liny-fetch-alias))
         (mode (liny-fetch-env-mode))
         (files (liny-intersection
                 (gethash alias liny-alias-index)
                 (liny-union-set (gethash mode liny-modes-index)
                                 (gethash "all" liny-modes-index))))
         (result (sort
                  (mapcar
                   (lambda(x)
                     (let* ((lst (gethash x liny-files-index))
                            (match (apply 'liny-keywords-match lst)))
                       (if match (cons match x)
                         (cons -1 x))))
                   files)
                  (lambda(x y)
                    (> (car x)(car y))))))
    (if result
        (setq liny-match-files result))
    (cdar result)))
