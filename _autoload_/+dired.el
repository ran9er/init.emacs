;; -*- encoding: utf-8-unix; -*-

;;;###autoload
(defun rename-file-specify-extension (&optional suf)
  (interactive)
  (let* (buffer-read-only
         (suffix (or suf "bak"))
         (from-file (dired-get-filename))
         (new-file 
          (if (string-equal suffix (file-name-extension from-file))
              (file-name-sans-extension from-file)
            (concat from-file "." suffix))))
    (if (file-exists-p  new-file)
        (delete-file new-file))
    (rename-file from-file new-file)
    (revert-buffer)))
