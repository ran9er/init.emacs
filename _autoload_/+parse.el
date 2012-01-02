;; -*- encoding: utf-8-unix; -*-
;; File-name:    <wordpress.el>
;; Create:       <2011-12-26 22:50:39 ran9er>
;; Time-stamp:   <2012-01-02 12:20:18 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun wordpress (beg end)
  (interactive "r")
  (mapc
   (lambda(x)
     (replace-string (car x) (cdr x) nil beg end))
   (alist '("\\"    "\\\\"
            "*"     "\\*"
            "_"     "\\_"
            "`"     "\\`"))))
