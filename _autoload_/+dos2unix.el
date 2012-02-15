;;;###autoload
(defun dos2unix ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))
    ;; (while (search-forward (string ?\C-m?\C-j) nil t)
    ;;   (replace-match (string ?\C-j) nil t))
    ))
