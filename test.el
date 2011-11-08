;; -*- encoding: utf-8-emacs-unix; -*-
(defun insert-doc-head ()
  (interactive)
  (let* ((cmnt (if (string= "" comment-end) comment-start))
         (common-head '(
                    "-*- encoding: utf-8-unix; -*-" "\n"
                    "Filename: " (if (buffer-file-name)
                    (file-name-nondirectory (buffer-file-name))) "\n"
                    "CreateTime: "
                    (format-time-string "%Y-%m-%d %a %H:%M:%S" (current-time)) "\n"
                    "Time-stamp: <>" "\n"
                    ))
;        (v (apply 'concat (cdr (assoc major-mode head-alist))))
         (v (eval `(concat ,@common-head
                           ,@(cdr (assoc major-mode head-alist)))))
         (o (apply 'concat 
          (mapcar
           (lambda(x)(concat comment-start cmnt " "
                             x comment-end "\n"))
           (split-string v "\n")))))
    (insert o)))

(setq head-alist '(
;                     (c-mode . ,common-head)
;                     (emacs-lisp-mode . ,common-head)
                     (emacs-lisp-mode . ("CreateTime: "
                                         (number-to-string(time-to-seconds))
                                         ))
;                     (lisp-interaction-mode . ,common-head)
;                     (ruby-mode . ,common-head)
                     ))

;(insert-doc-head)
;;CreateTime: 1320756076.64
;;-*- encoding: utf-8-emacs-unix; -*-
;;Filename: test.el
;;Time-stamp: <>


;(add-to-list 'head-alist '(m . n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if t t;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
     '(default ((t (:stipple nil :background ((image :type jpeg :file "/Path/to/your/image.png") :origin display) :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 101 :width normal :family "misc-fixed")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; * toggle-comment-region
;; (defun toggle-comment-region (beg end &optional n)
;;   "Comment the lines in the region if the first non-blank line is
;; commented, and conversely, uncomment region. If optional prefix arg
;; N is non-nil, then for N positive, add N comment delimiters or for N
;; negative, remove N comment delimiters.
;; Uses `comment-region' which does not place comment delimiters on
;; blank lines."
;;   (interactive "r\nP")
;;   (if n
;;       (comment-region beg end (prefix-numeric-value n))
;;     (save-excursion
;;       (goto-char beg)
;;       (beginning-of-line)
;;       ;; skip blank lines
;;       (skip-chars-forward " \t\n")
;;       (if (looking-at (concat "[ \t]*\\(" (regexp-quote comment-start) "+\\)"))
;;           (uncomment-region beg end)
;;         (comment-region beg end)))))
