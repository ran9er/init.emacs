;; -*- encoding: utf-8-unix; -*-
;; File-name:    <find-char-backward.el>
;; Create:       <2011-11-10 17:05:47 ran9er>
;; Time-stamp:   <2011-11-10 17:07:30 ran9er>
;; Mail:         <2999am@gmail.com>
(defun find-char-backward (charlst) 
  (interactive)
  (let ((q (point)))
  (while (null (member (char-to-string (get-byte (1- q))) charlst))
    (setq q (1- q))
    (goto-char q))
  (message "q")))
(def-k-s 1 "M-p" (lambda()(interactive)(find-backward '("w" ")"))))
