;; require smart-match

(add-hook 'liny-goto-field-nil-hook 'liny-multi-snippets-select)

(defun liny-multi-snippets-select ()
  "liny-multi-snippets-select is writen by ran9er"
  (interactive)
  (if (liny-get-overlay) (liny-clear-instance))
  (delete-region liny-expand-marker-beg liny-expand-marker-end)
  (setq liny-match-files
        (append (cdr liny-match-files)(list (car liny-match-files))))
  (liny-insert (cdar liny-match-files)))
