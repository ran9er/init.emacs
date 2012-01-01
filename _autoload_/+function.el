
;; -*- encoding: utf-8-unix; -*-
;; File-name:    <function.el>
;; Create:       <2011-12-27 21:33:05 ran9er>
;; Time-stamp:   <2012-01-01 13:07:39 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defvar temp-func-list
  '((let ((cnt 0)(acc nil))
      (mapc (lambda(x) (setq acc (concat acc "\n"
                    (number-to-string (setq cnt (1+ cnt)))
                    ": " (prin1-to-string  x ))))
            (butlast temp-func-list))
      acc)))

;;;###autoload
(defun temp-func-add (&optional beg end)
  (interactive "r")
  (let* (b e 
           (x (if mark-active (read (buffer-substring-no-properties beg end))
                (up-list)(setq e (point))
                (backward-list)(setq b (point))
                (forward-list)
                (read (buffer-substring-no-properties b e)))))
    (if (null (equal x (car temp-func-list)))
        (push x temp-func-list)))
  (deactivate-mark))

;;;###autoload
(defun temp-func-call (&optional n)
  (interactive "p")
  (message
   (pp-to-string
    (let ((func (if (eq n 0)
                    (car (last temp-func-list))
                  (nth (1- n) temp-func-list))))
      (if (functionp func)
          (funcall func)
        (eval func))))))

;;;###autoload
(defun set-local-variable(&optional v)
  (interactive "P")
  (let* ((n (if mark-active
                (buffer-substring-no-properties
                 (region-beginning) (region-end))
              (current-word t)))
         (v (eval
             (read
              (read-input (concat "set " n "'s value: ")))))
         (n (read n)))
    (make-local-variable n)
    (set n v)
    (message "set %s to %s" n v)))
