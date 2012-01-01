;; -*- encoding: utf-8-unix; -*-
;; * 定义函数
(defun set-outline-minor-mode ()
  (outline-minor-mode 1)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp
        (or
         (car (gethash major-mode *outline-minor-mode-hash*))
         outline-regexp))
  (make-local-variable 'outline-heading-alist)
  (setq outline-heading-alist
        (or 
         (cadr (gethash major-mode *outline-minor-mode-hash*))
         outline-heading-alist))
  (hide-body))

;; * 定义数据
;; (defvar *outline-minor-mode-hash* (make-hash-table :test 'equal :size 30))
;; (mapc (lambda (x)
;;         (puthash (car x)(cdr x)
;;                  *outline-minor-mode-hash*))
;;       (cons-list '(
;; lisp-interaction-mode        ";;;\\(;* [^ 	\n]\\|###autoload\\)\\|("
;; ;emacs-lisp-mode              ";;\\ \\*+\\|;;;\\(;* [^ 	\n]\\|###autoload\\)\\|("
;; emacs-lisp-mode              ";;\\ \\*+"
;; shell-mode                   ".*[bB]ash.*[#\$] \\|^.:\.*>"
;; sh-mode                      "function"
;; eshell-mode                  ".*[#\$]\\|^.:\.*>"
;; )))

(defvar *outline-minor-mode-hash*
  (mmkht ()
   lisp-interaction-mode
   (";;;\\(;* [^ 	\n]\\|###autoload\\)\\|(")
   emacs-lisp-mode
   (";;\\ \\*+\\|;;;\\(;* [^ 	\n]\\|###autoload\\)\\|("    ((";;\\ \\*+" . 1)))
   shell-mode
   (".*[bB]ash.*[#\$] \\|^.:\.*>")
   sh-mode
   ("function")
   eshell-mode
   (".*[#\$]\\|^.:\.*>")
   ))

;; * 应用数据
(mapc
 (lambda(h)
   (add-hook h 'set-outline-minor-mode t))
 (let (z)
   (maphash
    (lambda(x y)(setq z (cons (concat-symbol x "-hook") z)))
    *outline-minor-mode-hash*)
   z))

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
