;; -*- encoding: utf-8-unix; -*-
;; File-name:    <edit.el>
;; Create:       <2011-12-27 21:29:35 ran9er>
;; Time-stamp:   <2013-02-07 03:21:31 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun swap-point()
  (interactive)
  (if (or (null (boundp '*last-point*)) (null *last-point*))
      (progn (make-local-variable '*last-point*)
             (setq *last-point* (cons (point) (point))))
    (let ((p (point)))
      (if (eq p (cdr *last-point*))
          (progn (goto-char (car *last-point*))
                 (setq *last-point* (cons (cdr *last-point*)(car *last-point*))))
        (goto-char (cdr *last-point*))
        (setq *last-point* (cons p (cdr *last-point*)))))))

;;;###autoload
(defun beacon ()
  (interactive)
  (let ((k (where-is-internal 'beacon-jump)))
    (message (concat (mapconcat 'key-description k " , ")
                     (if k " or ")
                     "C-M-c to jump back.")))
  (let ((x (point-marker)))
    (catch 'exit (and (catch (recursion-depth) (recursive-edit)) (throw 'exit t)))
    (switch-to-buffer (marker-buffer x))(goto-char x)))

(defun beacon-jump (&optional n)
  (interactive "p")
  (let* ((x (recursion-depth))
         (i (if (> (or n 1) x)
                x n)))
    (throw (- x i) t)))

;;;###autoload
(defun resize-horizontal-space (&optional backward-only)
  (interactive "*P")
  (let ((orig-pos (point))
        (skip-chars " \t")
        (delimit-char
         (mapcar (lambda (x) (string-to-char x))
                 '("(" ")")))
        fwd-pos fwd-p bwd-pos bwd-p)
    (setq
     fwd-pos (progn (skip-chars-forward skip-chars)(eolp))
     fwd-p  (memq (following-char) delimit-char)
     bwd-pos (progn (skip-chars-backward skip-chars)(bolp))
     bwd-p  (memq (preceding-char) delimit-char))
    (goto-char orig-pos)
    (if (or fwd-pos bwd-pos (and fwd-p bwd-p))
        (delete-horizontal-space backward-only)
      (delete-horizontal-space backward-only)
      (insert " ")
      (if bwd-p (backward-char 1)))))

;;;###autoload
(defun smart-backward-kill ()
  (interactive)
  (let ((i (save-excursion (abs (skip-chars-backward " \t")))))
    (cond
     (mark-active
      (call-interactively 'kill-region))
     ((< 0 i)
      (backward-delete-char
       (if (zerop (mod i tab-width)) tab-width (mod i tab-width))))
     (t
      (call-interactively 'backward-kill-word)))))
