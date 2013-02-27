(defvar l-snippets-match-strategy 'l-snippets-smart-match)

(defvar l-snippets-env-test
  '((major-mode . major-mode)
    (head . (progn (skip-chars-backward " \t\n")(bobp)))
    (tail . (progn (skip-chars-forward " \t\n")(eobp)))
    (top . (eq (current-indentation) 0))
    ))

(defun l-snippets-fetch-env ()
  "l-snippets-fetch-env "
  (let ((varff))
  ))

(defun l-snippets-smart-match (str)
  (mapconcat 'identity
             (list
              (symbol-name major-mode)
              str)
             (plist-get
              l-snippets-syntax-meta
              'path-separator)))
