;; * match
(setq liny-match-strategy 'liny-smart-match)

(defvar liny-env-test
  `((,major-mode  major-mode t)
    (head (progn (skip-chars-backward " \t\n")(bobp)))
    (tail (progn (skip-chars-forward " \t\n")(eobp)))
    (notop (null (zerop (current-indentation))))
    (top (null (zerop (current-indentation))))
    ))

(defun liny-fetch-env ()
  "liny-fetch-env "
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
              liny-env-test))
            (lambda(x y)
              (string-lessp x y))))))
    (list (funcall test t)(funcall test nil))))

(defun liny-keywords-match (modes keywords)
  "liny-keywords-match is writen by ran9er"
  (let* (test
         (env (liny-fetch-env))
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
                (keywords (nth 1 lst)))
           (cons (liny-keywords-match modes keywords) x)))
       files)
      (lambda(x y)
        (> (car x)(car y)))))))
