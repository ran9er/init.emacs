;(setq c-tab-always-indent t)
;; 当光标在行尾上下移动的时候，始终保持在行尾。
;(setq track-eol t)

;;;###autoload
(defun my-auto-pair ()
        (interactive)
        (make-local-variable 'skeleton-pair-alist)
        (setq skeleton-pair-alist  '(
                                     ;; (?` _ "'")
                                     (?\( _ ")")
                                     (?\[ _ "]")
                                     (?\" _ "\"")
                                     (?{ \n > _ \n ?} >)
                                     ))
        (setq skeleton-pair t)
        (define-key-s 1  '("(" "{" "\"" "[") 'skeleton-pair-insert-maybe))

(defadvice skeleton-pair-insert-maybe (around condition activate)
  (let ((skeleton-pair-alist skeleton-pair-alist)
        (c-before
         (lambda(x)(save-excursion
                 (skip-chars-backward " \t")
                 (eq (char-before (point)) x)))))
    (if (and (eq last-command-event 123)(funcall c-before 61))
        (setq skeleton-pair-alist '((?\{ _ "}"))))
    ad-do-it))
