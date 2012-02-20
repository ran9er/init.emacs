;;;###autoload
(defun dos2unix ()
  "Convert a DOS formatted text buffer to UNIX format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;###autoload
(defun unix2dos ()
  "Convert a UNIX formatted text buffer to DOS format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;;;###autoload
(defun del^M ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))
    ;; (while (search-forward (string ?\C-m?\C-j) nil t)
    ;;   (replace-match (string ?\C-j) nil t))
    ))
