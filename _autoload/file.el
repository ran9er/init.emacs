;; -*- encoding: utf-8-unix; -*-
;; File-name:    <file.el>
;; Create:       <2011-12-27 21:32:02 ran9er>
;; Time-stamp:   <2011-12-27 21:32:24 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun find-temp (&optional suffix)
  (interactive "sExtension: ")
  (let ((suf (if (and suffix (null (string= suffix "")))
                 (concat "." suffix))))
    (find-file
     (concat
      (make-temp-name
       (expand-file-name
        (format-time-string "%Y%m%d%H%M%S-" (current-time))
        work-dir))
      suf))
    (run-hooks 'find-temp-hook)))

;;;###autoload
(defun write-temp (filename &optional confirm)
  (interactive
   (list (if buffer-file-name
             (read-file-name "Write file: "
                             nil nil nil nil)
           (read-file-name "Write file: " default-directory
                           (expand-file-name
                            (file-name-nondirectory (buffer-name))
                            default-directory)
                           nil nil))
         (not current-prefix-arg)))
  (let ((fnm buffer-file-name))
    (write-file filename confirm)
    (if (file-exists-p fnm)
        (delete-file fnm))))

(add-hook 'find-temp-hook (lambda ()
                            (yank)))
