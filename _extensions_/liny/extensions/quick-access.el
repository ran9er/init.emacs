(defun liny-quick-access (&optional snippet-name)
  "liny-quick-access is writen by ran9er"
  (interactive)
  (find-file
   (expand-file-name
    (or snippet-name (cdar liny-match-snippets))
    liny-repo))
  (add-hook 'before-save-hook
            'liny-quick-access-func t t))

(defun liny-quick-access-func ()
  "liny-quick-access-func is writen by ran9er"
  (liny-force-update-keyword))
