;; -*- encoding: utf-8-unix; -*-
;; File-name:    <+column.el>
;; Create:       <2012-02-10 13:39:11 ran9er>
;; Time-stamp:   <2012-02-10 13:39:23 ran9er>
;; Mail:         <2999am@gmail.com>
(defun thing-on-column (fn &optional n)
  (save-excursion
    (let ((c (current-column))
          (n (if mark-active
                 (prog1
                     (count-lines (region-beginning)(region-end))
                   (goto-char (region-beginning)))
               (or n 1)))
          lst)
      (while (> n 0)
        (setq n (1- n))
        (move-to-column c)
        (setq lst (cons (funcall fn) lst))
        (forward-line))
      (reverse lst))))

;;;###autoload
(defun copy-word-on-column (&optional n)
  (interactive "P")
  (kill-new
   (mapconcat
    'identity
    (thing-on-column 'current-word n)
    "\n")))

;;;###autoload
(defun copy-sexp-on-column (&optional n)
  (interactive "P")
  (kill-new
   (mapconcat
    'identity
    (thing-on-column
     (lambda()(buffer-substring-no-properties
           (progn (backward-sexp)(point))
           (progn (forward-sexp)(point)))) n) "\n")))
