;; -*- encoding: utf-8-unix; -*-
;; File-name:    <25_insert-head.el>
;; Create:       <2011-11-09 13:55:46 ran9er>
;; Time-stamp:   <2011-11-09 13:58:09 ran9er>
;; Mail:         <2999am@gmail.com>

(defun insert-doc-head ()
  (interactive)
  (let* (beg
        ;(cmnt (if (string= "" comment-end) comment-start))
         (common-head '(
          "-*- encoding: utf-8-unix; -*-" "\n"
          "File-name:    <" (if (buffer-file-name)
            (file-name-nondirectory (buffer-file-name))) ">\n"
          "Create:       <"
            (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)) 
            " " user-full-name ">\n"
          "Time-stamp:   <>" "\n"
          ))
;        (v (apply 'concat (cdr (assoc major-mode head-alist))))
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
;                     (c-mode . ,common-head)
;                     (emacs-lisp-mode . ,common-head)
                   (emacs-lisp-mode . ("Mail:         <"
                                       user-mail-address ">\n"
                                       ))
;                     (lisp-interaction-mode . ,common-head)
;                     (ruby-mode . ,common-head)
                     ))

;(insert-doc-head)
;(add-to-list 'head-alist '(m . n))

