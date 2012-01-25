;; -*- encoding: utf-8-unix; -*-
;; File-name:    <25_insert-head.el>
;; Create:       <2011-11-09 13:55:46 ran9er>
;; Time-stamp:   <2012-01-26 01:03:52 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun comment-block (&optional beg end)
  (interactive)
  (if mark-active
      (let* ((beg (region-beginning))
             (end (region-end))
             (s (buffer-substring-no-properties beg end))
             (o (if (equal (substring  s 0 2) "#@")
                    (substring s (+ 1 (string-match ":" s)))
                  s))
             (l (1+ (length o))))
        (delete-region beg end)
        (insert "#@" (number-to-string l) ":" o))))

;;;###autoload
(defun insert-autoload-magic-comment ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert ";;;###autoload\n")))

;;;###autoload
(defun insert-time (&optional format)
  (interactive )
  (insert
   (format-time-string
    (or format "%Y-%m-%d <%u> %H:%M:%S")
    (current-time))))

;;;###autoload
(defun insert-delimit-line (&optional n)
  (interactive "P")
  (let* ((ts "--%Y-%m-%d--%u--%H:%M:%S--")
         (fts (format-time-string ts (current-time)))
         (len (+ (length fts) 6))
         (n (if (and n (> n len)) n len))
         (x (/ (- n len) 2))
         (y (- n x len)))
    (insert
     "->>"
     (make-string x ?-)
     fts
     (make-string y ?-)
     "-->")))

;;;###autoload
(defun insert-doc-head ()
  (interactive)
  (let* (beg
         ;; (cmnt (if (string= "" comment-end) comment-start))
         (common-head '(
          "-*- encoding: utf-8-unix; -*-" "\n"
          "File-name:    <" (if (buffer-file-name)
            (file-name-nondirectory (buffer-file-name))
            (buffer-name)) ">\n"
          "Create:       <"
            (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
            " " user-full-name ">\n"
          "Time-stamp:   <>" "\n"
          ))
         ;; (v (apply 'concat (append common-head (cdr (assoc major-mode head-alist))))))
         (v (eval `(concat ,@common-head
                           ,@(cdr (assoc major-mode head-alist))))))
         ;; (o (apply 'concat
         ;;  (mapcar
         ;;   (lambda(x)(concat comment-start cmnt " "
         ;;                     x comment-end "\n"))
         ;;   (split-string v "\n")))))
    (setq beg (point))
    (insert v)
    (comment-region beg (point))
    ))


(setq head-alist '(
                   ;; (c-mode . ,common-head)
                   ;; (emacs-lisp-mode . ,common-head)
                   (emacs-lisp-mode . ("Mail:         <"
                                       user-mail-address ">\n"
                                       ))
                   ;; (lisp-interaction-mode . ,common-head)
                   ;; (ruby-mode . ,common-head)
                   ))

;(insert-doc-head)
;(add-to-list 'head-alist '(m . n))

