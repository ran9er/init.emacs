;; -*- encoding: utf-8-emacs-unix; -*-
;; background
(when nil
  (custom-set-faces
   '(default
      ((t (:background
           ((image
             :type jpeg
             :file "/Path/to/your/image.png")
            :origin display)
           :stipple nil
           :foreground "white"
           :inverse-video nil
           :box nil
           :strike-through nil
           :overline nil
           :underline nil
           :slant normal
           :weight normal
           :height 101
           :width normal
           :family "misc-fixed")))))

  (setq acc 0)

  (dolist (x (loop for i upto 99 collect i))
    (insert " ")
    (set-text-properties
     (1- (point)) (point)
     `(font-lock-face 
       (:background
        ,(adjust-color "gray20" x)))))

  (loop collect i for i upto 99)
  )

(defun function-maybe (fn)
  (if (functionp fn)
      fn
    (let ((s "-"))
      (intern
       (mapconcat
        'identity
        (cdr (split-string (symbol-name fn) s)) s)))))

(defadvice newsticker-treeview (before rss activate)
  (load1 (expand-file-name "my-newsticker.el" work-dir)))

(eval-after-load 'eshell
  '(progn
     (defvar acc 0)
     (incf acc)))
