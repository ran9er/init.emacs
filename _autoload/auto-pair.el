;(setq c-tab-always-indent t)
;; 当光标在行尾上下移动的时候，始终保持在行尾。
;(setq track-eol t)

;;;###autoload
(defun my-auto-pair ()
        (interactive)
        (make-local-variable 'skeleton-pair-alist)
        (setq skeleton-pair-alist  '(
                                     (?` _ "'")
                                     (?\(  _ ")")
                                     (?\[  _ "]")
                                     (?{ \n > _ \n ?} >)
                                     ))
        (setq skeleton-pair t)
        (define-key-s 1  '("(" "{" "`" "[") 'skeleton-pair-insert-maybe))
