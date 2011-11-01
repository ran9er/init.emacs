;; -*- encoding: utf-8-unix; -*-
;; * 定义函数
(defun set-outline-minor-mode-regexp ()
  ""
  (outline-minor-mode 1)
  (let ((regexp-list (append outline-minor-mode-list nil))
	(find-regexp
	 (lambda (lst)
	   ""
	   (let ((innerList (car lst)))
	     (if innerList
		 (if (string= (car innerList) major-mode)
		     (car (cdr innerList))
		   (progn (pop lst)
			  (funcall find-regexp lst))))
	     ))))
    (make-local-variable 'outline-regexp)
    (setq outline-regexp (funcall find-regexp regexp-list)))
  
;  (define-key-bindings 1 `(
;		     ("C-c C-t" hide-body)
;		     ("C-c C-a" show-all)
;		     ("C-c C-e" show-entry)
;		     ))

  (hide-body)
  )

;; * 定义数据
(setq outline-minor-mode-list 
      (list 
       '(lisp-interaction-mode "(")
       '(emacs-lisp-mode ";;\\ \\*+")
       '(shell-mode ".*[bB]ash.*[#\$] \\|^.:\.*>")
       '(sh-mode "function")
;      '(eshell-mode ".*[#\$]\\|^.:\.*>")
       )
      )

;; * 应用数据
(dolist (hook (mapcar
               (lambda(lst) (read (concat (symbol-name (car lst)) "-hook")))
               outline-minor-mode-list))
  (add-hook hook 'set-outline-minor-mode-regexp t))

;; * 定义键位
(setq outline-minor-mode-prefix [(control t)])
;; * 自定义省略提示
;; ** 字符
; (set-display-table-slot standard-display-table 
;                         'selective-display 
;                         (string-to-vector " [...] "))
;; ** 主题
(set-display-table-slot
 standard-display-table
 'selective-display
 (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
   (vconcat (mapcar (lambda (c) (+ face-offset c)) " {...} "))))

;; * outline-magic
;; (add-hook 'outline-mode-hook
;;           (lambda ()
;;             (require 'outline-cycle)))

(add-hook 'outline-minor-mode-hook 
          (lambda () 
            (require 'outline-magic)
            ;; 定义快捷键 tab | f1
            (define-key outline-minor-mode-map [(f1)] 'outline-cycle)))

;; 在正文中模拟tab，用tab作快捷键时开启
;(setq outline-cycle-emulate-tab t)

