;; -*- encoding: utf-8-unix; -*-
;; File-name:    <del-tail-spc.el>
;; Create:       <2011-11-19 16:06:15 ran9er>
;; Time-stamp:   <2011-11-19 16:06:20 ran9er>
;; Mail:         <2999am@gmail.com>
;; * del-tail-spc
;; (delete-trailing-whitespace)
(defun del-tail-spc()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((n (line-number-at-pos (point-max))))
      (while (<= 0 (setq n (1- n)))
        (goto-char (line-end-position))
        (delete-horizontal-space)
        (forward-line)
       ))))

