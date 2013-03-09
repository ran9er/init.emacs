;; * match
(setq liny-match-strategy 'liny-smart-match)

(defvar liny-env-test-necessary
  '())

(defvar liny-env-test-sufficient
  '((head (progn (skip-chars-backward " \t\n")(bobp)))
    (tail (progn (skip-chars-forward " \t\n")(eobp)))
    (notop (null (zerop (current-indentation))))
    (top (null (zerop (current-indentation))))))

(defun liny-fetch-env-sufficient ()
  "liny-fetch-env "
  (let ((test
         (lambda(tst)
           (sort
            (remove
             nil
             (mapcar
              (lambda(x)
                (if (save-excursion (save-restriction (eval (nth 1 x))))
                    (cons (symbol-name (nth 0 x)) (or (nth 2 x) 1))))
              tst))
            (lambda(x y)
              (string-lessp x y))))))
        (funcall test liny-env-test-sufficient)))

(defun liny-fetch-env-mode ()
  "liny-fetch-env-mode is writen by ran9er"
  major-mode)

(defun liny-keywords-match (&optional modes keywords)
  "liny-keywords-match is writen by ran9er"
  (let* (test
         (env (liny-fetch-env-sufficient))
         (result 0))
    (if (or (member "all" modes)
            (member (symbol-name (liny-fetch-env-mode)) modes))
        (setq test t))
    (if test
        (mapc
         (lambda(x)
           (if (member (car x) keywords)
               (setq result (+ (cdr x) result))))
         env))
    (and test result)))

;; * index
(defun liny-alias-push (var alias files)
  (let ((a (assoc alias var)))
    (if (null a)
        (cons (list alias files) var)
      (if (member files (cdr a))
          var
        (let* ((n (cons files (cdr a)))
               (n (cons alias n)))
          (remove a (cons n var)))))))

(defun liny-gen-index-k ()
  (let ((gs (lambda(x)(sort (remove "" (if x (split-string x "[ \t\n]"))) 'string-lessp)))
        alias files)
    (setq
     files
     (mapcar
      (lambda(x)
        (with-temp-buffer
          (insert-file-contents (expand-file-name x liny-repo))
          (mapc (lambda(y)(setq alias (liny-alias-push alias y x)))
                (funcall gs (liny-search-str "alias")))
          (list
           x
           (funcall gs (liny-search-str "modes"))
           (funcall gs (liny-search-str "keywords")))))
      (directory-files liny-repo nil "^[^._].*\\'")))
    (cons alias files)))

(defun liny-snippet-exist-p (snippet)
  (assoc snippet (cdr liny-index)))

(let (print-length print-level selective-display-ellipses)
  (liny-update-index "_keywords_index"
                           (pp-to-string (liny-gen-index-k))))

(defun liny-force-update-keyword ()
  (interactive)
  (let (print-length print-level selective-display-ellipses)
    (liny-update-index
     "_keywords_index"
     (pp-to-string (liny-gen-index-k))
     t)))

;; (insert (concat "\n" (pp-to-string (liny-gen-index-k))))


;; *
(defun liny-smart-match ()
  (let* ((alias (liny-fetch-alias))
         (files (cdr (assoc alias (car liny-index)))))
    (cdar
     (sort
      (mapcar
       (lambda(x)
         (let* ((lst (cdr (assoc x (cdr liny-index))))
                (modes (nth 0 lst))
                (keywords (nth 1 lst))
                (match (liny-keywords-match modes keywords)))
           (if match (cons match x))))
       files)
      (lambda(x y)
        (> (car x)(car y)))))))
