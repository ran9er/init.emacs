;;;###autoload
(defun dos2unix (buffer)
      "Automate M-% C-q C-m RET C-q C-j RET"
      (interactive "*b")
      (save-excursion
        (goto-char (point-min))
        (while (search-forward (string ?\C-m?\C-j) nil t)
          (replace-match (string ?\C-j) nil t))))
