;; -*- encoding: utf-8-unix; -*-
;; File-name:    <nxml.el>
;; Create:       <2011-12-24 14:21:54 ran9er>
;; Time-stamp:   <2011-12-29 11:19:52 ran9er>
;; Mail:         <2999am@gmail.com>
(load-once
 (defun nxml-outside (&optional n)
   (interactive "P")
   (let ((x (if n (prefix-numeric-value n) 1))
         beg end tmp)
     (if mark-active
         (setq beg (region-beginning)
               end (region-end)
               end (progn (goto-char (1+ end))(point-marker)))
       (nxml-up-element x)
       (setq end (progn (forward-char 1)(point-marker)))
       (setq beg (progn (nxml-backward-element)(point)))
       )
     (setq tmp (buffer-substring-no-properties beg (1- end)))
     (delete-region beg (1- end))
     (insert "<TAG></TAG>")
     (backward-char 6)
     (save-excursion
       (insert tmp))
     (backward-char 4)
     (parallel-edit (list (- end 5)) 3)))

(add-hook
 'nxml-mode-hook
 (def-k-s nxml-mode-map
   "C-j"    nxml-balanced-close-start-tag-inline
   "M-j"    nxml-balanced-close-start-tag-block
   "C-9"    nxml-outside))
);load1
