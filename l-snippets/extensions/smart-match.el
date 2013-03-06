;; * match
(setq l-snippets-match-strategy 'l-snippets-smart-match)

(defvar l-snippets-env-test
  `((,major-mode  major-mode t)
    (head (progn (skip-chars-backward " \t\n")(bobp)))
    (tail (progn (skip-chars-forward " \t\n")(eobp)))
    (notop (null (zerop (current-indentation))))
    (top (null (zerop (current-indentation))))
    ))

(defun l-snippets-fetch-env ()
  "l-snippets-fetch-env "
  (let ((test
         (lambda(s)
           (sort
            (remove
             nil
             (mapcar
              (lambda(x)
                (if (and
                     (eq (nth 2 x) s)
                     (save-excursion (save-restriction (eval (nth 1 x)))))
                    (symbol-name (nth 0 x))))
              l-snippets-env-test))
            (lambda(x y)
              (string-lessp x y))))))
    (list (funcall test t)(funcall test nil))))

(defun l-snippets-keywords-match (modes keywords)
  "l-snippets-keywords-match is writen by ran9er"
  (let* (test
         (env (l-snippets-fetch-env))
         (test1 (nth 0 env))
         (test2 (nth 1 env))
         (result 0))
    (if (member "all" modes)
        (setq test t)
      (while
          (and test1
               (null (if (member (car test1) modes)
                   (setq test t))))
        (setq test1 (cdr test1))))
    (if test
        (mapc
         (lambda(x)
           (if (member x keywords)
               (setq result (1+ result))))
         test2))
    (and test result)))

;; * index
(defun l-snippets-alias-push (var alias files)
  (let ((a (assoc alias var)))
    (if (null a)
        (cons (list alias files) var)
      (if (member files (cdr a))
          var
        (let* ((n (cons files (cdr a)))
               (n (cons alias n)))
          (remove a (cons n var)))))))

(defun l-snippets-gen-index-k ()
  (let ((gs (lambda(x)(sort (remove "" (if x (split-string x "[ \t\n]"))) 'string-lessp)))
        alias files)
    (setq
     files
     (mapcar
      (lambda(x)
        (with-temp-buffer
          (insert-file-contents (expand-file-name x l-snippets-repo))
          (mapc (lambda(y)(setq alias (l-snippets-alias-push alias y x)))
                (funcall gs (l-snippets-search-str "alias")))
          (list
           x
           (funcall gs (l-snippets-search-str "modes"))
           (funcall gs (l-snippets-search-str "keywords")))))
      (directory-files l-snippets-repo nil "^[^._].*\\'")))
    (cons alias files)))

(defun l-snippets-snippet-exist-p (snippet)
  (assoc snippet (cdr l-snippets-index)))

(l-snippets-update-index "_keywords_index"
                         (pp-to-string (l-snippets-gen-index-k)))

(defun l-snippets-force-update-keyword ()
  (interactive)
  (l-snippets-update-index
   "_keywords_index"
   (pp-to-string (l-snippets-gen-index-k))
   t))

;; (insert (concat "\n" (pp-to-string (l-snippets-gen-index-k))))


;; *
(defun l-snippets-smart-match ()
  (let* ((alias (l-snippets-fetch-alias))
         (files (cdr (assoc alias (car l-snippets-index)))))
    (cdar
     (sort
      (mapcar
       (lambda(x)
         (let* ((lst (cdr (assoc x (cdr l-snippets-index))))
                (modes (nth 0 lst))
                (keywords (nth 1 lst)))
           (cons (l-snippets-keywords-match modes keywords) x)))
       files)
      (lambda(x y)
        (> (car x)(car y)))))))
