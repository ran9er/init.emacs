;; -*- encoding: utf-8-unix; -*-
;; File-name:    <beacon.el>
;; Create:       <2013-02-07 03:11:59 ran9er>
;; Time-stamp:   <2013-02-07 03:12:02 ran9er>
;; Mail:         <2999am@gmail.com>
(defun beacon (&optional n)
  (interactive)
  (if (null (eq last-command 'beacon-jump))
      (let ((k (where-is-internal 'beacon-jump)))
        (message (concat (mapconcat 'key-description k " , ")
                         (if k " or ")
                         "C-M-c to jump back."))))
  (let ((x (point-marker)))
    (if (and n (> (recursion-depth) 0))
        (throw (- (recursion-depth) n) t)
      (catch 'exit (and (catch (recursion-depth) (recursive-edit)) (throw 'exit t)))
      (goto-char x))))

(defun beacon-jump (&optional n)
  (interactive "p")
  (let ((x (recursion-depth)))
    (beacon (if (> (or n 1) x)
                x n))))
