(setq v load-file-name)
(setq x (make-overlay 1 1))
(move-overlay x 3894 3898)
(delete-overlay x)
(overlay-put x 'face hl-line-face)
(overlay-put x 'after-string s)
(overlay-put x 'invisible nil)
(setq s "")
(setq s (propertize s 'face hl-line-face))

(defvar multiwidth-space-list
  (list
   ?\t
   (decode-char 'ucs #x3000)		; japanese fullwidth space
   ))

(defun whit-column (col how &optional ovl)
  (let* ((o (or ovl (make-overlay 1 1)))
         )
    ;; initialize overlay.
    (overlay-put o 'face nil)
    (overlay-put o 'before-string nil)
    (overlay-put o 'after-string nil)
    (overlay-put o 'invisible nil)

    ))

(defun indent-hint-white-line (&optional n)
  (save-excursion
    (let* ((i (current-indentation))
           (y (eq i (progn
                      (goto-char (line-end-position))
                      (current-column)))))
      (if (and y (> n i))
          (insert (make-string (- n i) 32)))
      y)))
