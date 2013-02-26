(defvar l-snippets-match-strategy 'smart-match)

(defun smart-match (str)
  (mapconcat 'identity 
             (list 
              (symbol-name major-mode)
              str)
             (plist-get
              l-snippets-syntax-meta
              'path-separator)))

