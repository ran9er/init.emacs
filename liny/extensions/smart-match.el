;; * match
(setq liny-match-strategy 'liny-smart-match)

(defvar liny-alias-index
  (make-hash-table :test 'equal))

(defvar liny-files-index
  (make-hash-table :test 'equal))

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
  major-mode)

(defun liny-keywords-match (&optional modes necessary sufficient)
  "liny-keywords-match is writen by ran9er"
  (let* ((env (liny-fetch-env))
         (result 0)
         envl)
    (and
     (catch 'test
       (or (member "all" modes)
           (member (symbol-name (liny-fetch-env-mode)) modes)
           (throw 'test nil))
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

;; * index
(defun liny-alias-push (alias file hash)
  (let ((v (gethash alias hash)))
    (if (null (member file v))
        (puthash alias (sort (append (list file) v) 'string-lessp) hash))))


(defun liny-gen-index-k ()
  (let ((gs
         (lambda(x)
           (sort
            (remove "" (if x (split-string x "[ \t\n]")))
            'string-lessp)))
        (alias (make-hash-table :test 'equal))
        (files (make-hash-table :test 'equal)))
    (mapc
     (lambda(x)
       (with-temp-buffer
         (let (necessary sufficient)
           (insert-file-contents (expand-file-name x liny-repo))
           (mapc (lambda(y)(liny-alias-push y x alias))
                 (funcall gs (liny-search-str "alias")))
           (mapc
            (lambda(x)
              (if (equal "+" (substring x 0 1))
                  (setq necessary (cons (substring x 1) necessary))
                (setq sufficient (cons x sufficient))))
            (funcall gs (liny-search-str "keywords")))
           (puthash
            x
            (list
             (funcall gs (liny-search-str "modes"))
             necessary
             sufficient)
            files))))
     (directory-files liny-repo nil "^[^._].*\\'"))
    (cons alias files)))

(defun liny-snippet-exist-p (snippet)
  (gethash snippet liny-files-index))

(defun liny-update-keyword-index (file strs &optional force)
  "liny-update-keyword-dir is writen by ran9er"
  (let* ((lst (liny-read-index (liny-update-index-dir file strs force))))
    (setq liny-alias-index (car lst)
          liny-files-index (cdr lst))))

(liny-update-keyword-index "_keywords_index" '(liny-gen-index-k))

(defun liny-force-update-keyword ()
  (interactive)
  (liny-update-keyword-index "_keywords_index" '(liny-gen-index-k) t))

;; (insert (concat "\n" (pp-to-string (liny-gen-index-k))))


;; *
(defun liny-smart-match ()
  (let* ((alias (liny-fetch-alias))
         (files (gethash alias liny-alias-index)))
    (cdar
     (sort
      (mapcar
       (lambda(x)
         (let* ((lst (gethash x liny-files-index))
                (match (apply 'liny-keywords-match lst)))
           (if match (cons match x))))
       files)
      (lambda(x y)
        (> (car x)(car y)))))))
