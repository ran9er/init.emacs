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
(defun 2list (lst)
  (if lst
      (cons
       (list (car lst)(cadr lst))
       (2list (cddr lst)))))

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
        (defs (if (eq group nil)
                  (2list key-defs)
                (mapcar (lambda (k) (list k group)) key-defs))))
    (mapc
     (lambda (d) (define-key map (eval `(kbd ,(car d))) (cadr d)))
     defs)))

(defmacro def-k-s (km &rest kd)
  "(def-key-s map \"key\" def \"key\" def ...)
See also `define-key-s'."
;  (list 'define-key-s km `',kd))
  `(define-key-s ,km ',kd))

;; * backward-kill-word-or-kill-region
(defun backward-kill-word-or-kill-region ()
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'backward-kill-word)))

;; * toggle-comment-region
(defun toggle-comment-region (beg end &optional n)
  "Comment the lines in the region if the first non-blank line is
commented, and conversely, uncomment region. If optional prefix arg
N is non-nil, then for N positive, add N comment delimiters or for N
negative, remove N comment delimiters.
Uses `comment-region' which does not place comment delimiters on
blank lines."
  (interactive "r\nP")
  (if n
      (comment-region beg end (prefix-numeric-value n))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      ;; skip blank lines
      (skip-chars-forward " \t\n")
      (if (looking-at (concat "[ \t]*\\(" (regexp-quote comment-start) "+\\)"))
          (uncomment-region beg end)
        (comment-region beg end)))))
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

