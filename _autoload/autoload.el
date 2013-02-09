;; -*- encoding: utf-8-unix; -*-
;; File-name:    <+autoload.el>
;; Create:       <2012-02-28 22:10:29 ran9er>
;; Time-stamp:   <2012-02-28 22:10:46 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun uatld (dir &optional loaddefs basedir)
  (let* ((path
          (expand-file-name dir basedir))
         (ldfs
          (or loaddefs "_loaddefs"))
         (generated-autoload-file
          (expand-file-name ldfs path)))
    (update-directory-autoloads path)
    (kill-buffer ldfs)
    (load generated-autoload-file)))
