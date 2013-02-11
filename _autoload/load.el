;; -*- encoding: utf-8-unix; -*-
;; File-name:    <+load.el>
;; Create:       <2012-03-10 22:13:02 ran9er>
;; Time-stamp:   <2013-02-11 10:59:53 ran9er>
;; Mail:         <2999am@gmail.com>

(defvar *load-times* (make-hash-table :test 'equal :size 20))
;;;###autoload
(defmacro load-once (&rest s)
  (let* ((hash *load-times*)
         (name
          (or load-file-name (buffer-file-name))))
    `(if (gethash ,name ,hash)
         (puthash
          ,name
          (1+ (gethash ,name ,hash))
          ,hash)
       ,@s
       (puthash ,name 1 ,hash))))

;;;###autoload
(defun load1 (file)
  (let ((hash *load-times*)
        (name (expand-file-name file)))
    (if (gethash name hash)
        (puthash name (1+ (gethash name hash)) hash)
      (load file)
      (puthash name 1 hash))))
