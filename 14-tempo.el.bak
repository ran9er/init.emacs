(require 'tempo-snippets)
(when t
  (defvar tempo-snippets-source-map (make-sparse-keymap))
  (define-key tempo-snippets-source-map (kbd "TAB") 'tempo-snippets-next-field)
  (define-key tempo-snippets-source-map (kbd "S-TAB") 'tempo-snippets-previous-field)
  (define-key tempo-snippets-source-map (kbd "C-m") 'tempo-snippets-clear-latest)

  (defadvice tempo-snippets-finish-source (before clear-post-overlay (o) act)
    (delete-overlay (overlay-get o 'tempo-snippets-post)))

  (defadvice tempo-snippets-insert-source (after install-custom-map act)
    (let ((overlay (car tempo-snippets-sources)))
      (overlay-put overlay 'keymap tempo-snippets-source-map)
      (overlay-put overlay 'tempo-snippets-post (point))))

  (defadvice tempo-snippets-insert-template (after install-post-map act)
    (dolist (s tempo-snippets-sources)
      (let ((pos (overlay-get s 'tempo-snippets-post)))
        (when (integerp pos)
          (let ((o (make-overlay pos (1+ pos))))
            (overlay-put o 'keymap tempo-snippets-source-map)
            (overlay-put s 'tempo-snippets-post o)))))
    ad-return-value)
  )
;; * sample
(when nil
  (tempo-define-snippet "java-get-set"
    '("private " (p "Type: " type) " _" (p "Name: " var) ";\n\n"
      > "public " (s type) " get" (upcase-initials (tempo-lookup-named 'var))
      "() {\n"
      > "return _" (s var)  ";\n" "}" > n n
      > "public set" (upcase-initials (tempo-lookup-named 'var))
      "(" (s type) " value) {\n"
      > "_" (s var) " = value;\n" "}" > n))

  (tempo-define-snippet "c-for-it"
    '(> "for (" (p "Type: " type) "::iterator " (p "Iterator: " it) " = "
        (p "Container: " container) ".begin();" n>
        (s it) " != " (s container) ".end(); ++" (s it) ") {" > n> & n "}" >)
    "fori"
    "Insert a C++ for loop iterating over an STL container."
    nil)
  )
