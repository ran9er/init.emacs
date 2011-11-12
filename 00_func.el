;; -*- encoding: utf-8-unix; -*-
;; * add-to-list-x
(defun add-to-list-x (LIST-VAR &rest REST)
"See also `add-to-list-l' `add-to-list-p'

\(add-to-list-x 'load-path
               init-dir
               (expand-file-name \"_misc/\" init-dir)
               )"
  (mapc (lambda(ELEMENT) (add-to-list LIST-VAR ELEMENT)) REST))

(defun add-to-list-l (LIST-VAR LIST)
"See also `add-to-list-x'"
  (apply 'add-to-list-x LIST-VAR LIST))

(defun add-to-list-p (LIST-VAR &optional BASE &rest REST)
"See also `add-to-list-x'"
  (mapc (lambda(ELEMENT) (add-to-list LIST-VAR (expand-file-name ELEMENT BASE))) REST))

;; * rq-x
(defun rq-x (action lst)
  "(rq-x 'require
        '(aaa bbb ccc ...))"
  (let ((action (cond ((eq action 0) 'require)(t action))))
    (mapcar (lambda(ext) (funcall action ext)) lst)))

(defmacro rqx (action &rest lst)
  "(rqx 0 aaa bbb ccc)"
;  (list 'rq-x `',action `',lst))
  `(rq-x ',action ',lst))

;; * define-key-s
(defun cons-list (lst)
  (if lst
      (cons
       (cons (car lst)(cadr lst))
       (cons-list (cddr lst)))))

(defun define-key-s (keymap key-defs &optional group)
  "(define-key-s 0 '(\"key\" def \"key\" def ...))
\(define-key-s 0 '(\"a\" \"b\" \"c\" ...) 'self-insert-command)
If keymap is 0, run as global-set-key
If keymap is 1, run as local-set-key
If keymap is xxx-mode-map, run as define-key xxx-mode-map
See also `def-key-s'."
  (let ((map (cond
              ((eq keymap 0) (current-global-map))
              ((eq keymap 1) (current-local-map))
              (t keymap)))
        (defs (if (null group)
                  (cons-list key-defs)
                (mapcar (lambda (k) (cons k group)) key-defs))))
    (mapc
     (lambda (d) (define-key map (eval `(kbd ,(car d))) (cdr d)))
     defs)))

(defmacro def-k-s (km &rest kd)
  "(def-key-s map \"key\" def \"key\" def ...)
See also `define-key-s'."
;  (list 'define-key-s km `',kd))
  `(define-key-s ,km ',kd))

(defun def-key-s (keymap &rest key-defs)
  ;; 对参数求值
  "(def-key-s map \"key\" 'def \"key\" 'def ...)
See also `define-key-s'."
  (define-key-s keymap key-defs))

;; * backward-kill-word-or-kill-region
(defun backward-kill-word-or-kill-region ()
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'backward-kill-word)))

;; * outside
(defmacro outside (pre suf m)
  "up list N level, append PRE ahead and SUF behind, backward M char"
  `(lambda(&optional n)
     (interactive "P")
     (let ((x (if n (prefix-numeric-value n) 1))
           p)
       (up-list x)
       (setq p (point))
       (insert ,suf)
       (goto-char p)
       (setq p (backward-list))
       (while (member (char-to-string (get-byte (1- p)))
                      '("'" "`" "," "#"))
         (setq p (1- p)))
       (goto-char p)
       (insert ,pre)
       (backward-char ,m)
       )))
;(def-key-s 0 "C-9" (outside "()" 1))

;; * shell-command-symbol-to-string
(defmacro shell-command-symbol-to-string (&rest s)
  `(shell-command-to-string
    (apply 'concat (mapcar
     (lambda(x)(concat (symbol-name x) " "))
     ',s))))
(defalias 'ss 'shell-command-symbol-to-string)

;; * del-tail-spc
(defun del-tail-spc()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((n (line-number-at-pos (point-max))))
      (while (<= 0 (setq n (1- n)))
        (goto-char (line-end-position))
        (delete-horizontal-space)
        (forward-line)
       ))))

;; * substring-buffer-name
(defun substring-buffer-name (m n &optional x)
  "使用 substring 截取文件名时，在 buffer-name 后面加几个字符，\
防止文件名过短引发错误。m n 参数同`substring'的 from to，可选参数\
 x 存在时截取带路径的文件名。"
  (substring (concat
              (if x
                  (buffer-file-name)
                (buffer-name))
              (make-string n ?*))
             m n))

;; * test
(defun mklst (n)
  "创建大小为 n 的字符串列表"
  (let* ((i n)(x nil))
    (while (> i 0)
      (setq x (cons (number-to-string i) x))
      (setq i (1- i)))
    x))

(defmacro test-list (n &rest fn)
  "用大小为 n 的字符串列表，测试函数 fn (fn 最后一个参数为列表)"
  `(,@fn (mklst ,n)))

(defmacro test-times (n &rest body)
  "计算 body 运行 n 次所需时间"
  `(let ((tm ,n)(beg (float-time)))
     (while (> tm 0)
       (progn ,@body)
       (setq tm (1- tm)))
     (- (float-time) beg)
     ))

;(test-times 100 (test-list 9 define-key-s (current-local-map)))
