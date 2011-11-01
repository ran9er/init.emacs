;; -*- encoding: utf-8-unix; -*-
(autoload 'dired-update-file-line "dired-aux" "\
Not documented
\(fn FILE)" t nil)


(defun rename-file-specify-extension ()
  (interactive)
  (let* (buffer-read-only
         (suffix "bak")
         (from-file (dired-get-filename))
         (new-file 
          (if (string-equal suffix (file-name-extension from-file))
              (file-name-sans-extension from-file)
            (concat from-file "." suffix))))
    (if (file-exists-p  new-file)
        (progn (delete-file new-file)
               (rename-file from-file new-file))
      (rename-file from-file new-file))
    (revert-buffer)
    ))

