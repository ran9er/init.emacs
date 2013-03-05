(defvar l-snippets-match-strategy 'l-snippets-smart-match)

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

(defun l-snippets-keywords-match (keywords)
  "l-snippets-keywords-match is writen by ran9er"
  (let* (test
         (env (l-snippets-fetch-env))
         (test1 (nth 0 env))
         (test2 (nth 1 env))
         (result 0))
    (if (member "all" keywords)
        (setq test t)
      (while
          (and test1
               (if (member (car test1) keywords)
                   (setq test t)))
        (setq test1 (cdr test1))))
    (if test
        (mapc
         (lambda(x)
           (if (member x keywords)
               (setq result (1+ result))))
         test2))
    (and test result)))


(defun l-snippets-smart-match (str)
  (mapconcat 'identity
             (list
              (symbol-name major-mode)
              str)
             (plist-get
              l-snippets-syntax-meta
              'path-separator)))
