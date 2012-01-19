;; -*- encoding: utf-8-unix; -*-
;; * load-once
(defvar *load-times* (make-hash-table :test 'equal :size 20))
(defmacro load-once (&rest s)
  (let* ((hash *load-times*)
         (name
          (or load-file-name (buffer-file-name))))
    `(if (gethash ,name ,hash)
         (puthash
          ,name
          (1+ (gethash ,name ,hash))
          ,hash)
       ,@s
       (puthash ,name 1 ,hash))))

(defun load1 (file)
  (let ((hash *load-times*)
        (name (expand-file-name file)))
    (if (gethash name hash)
        (puthash name (1+ (gethash name hash)) hash)
      (load file)
      (puthash name 1 hash))))

;; * list-to-alist
(defun to-alist (lst)
  "(to-alist '(1 2 3 4 5 6)) => ((1 . 2) (3 . 4) (5 . 6))"
  (if lst
      (cons
       (cons (car lst)(cadr lst))
       (to-alist (cddr lst)))))

;; * make hash-table
(defun mkhtb (&rest rest)
  (let* ((lst (if (eq (logand (length rest) 1) 1)
                  `[,@rest nil]
                `[,@rest]))
         (cnt (/ (length lst) 2))
         (size (+ cnt 2 (/ cnt 5)))
         (h (make-hash-table :test 'equal :size size)))
    (while (> cnt 0)
      (puthash
       (aref lst (- (* cnt 2) 2)) (aref lst (- (* cnt 2) 1)) h)
      (setq cnt (1- cnt)))
    h))
(defmacro mkht (&rest rest)
  `(apply 'mkhtb '(,@rest)))

;; * concat symbol
(defun concat-symbol (&rest lst)
  (intern (apply 'concat (mapcar (lambda(x)(if (symbolp x) (symbol-name x) x)) lst))))

;; * define-key-s
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
                  (to-alist key-defs)
                (mapcar (lambda (k) (cons k group)) key-defs))))
    (mapc
     (lambda (d) (define-key map (eval `(kbd ,(car d))) (cdr d)))
     defs)))

(defmacro def-k-s (km &rest kd)
  "(def-key-s map \"key\" def \"key\" def ...)
See also `define-key-s'."
  ;; (list 'define-key-s km `',kd))
  `(define-key-s ,km ',kd))

(defun def-key-s (keymap &rest key-defs)
  ;; 对参数求值
  "(def-key-s map \"key\" 'def \"key\" 'def ...)
See also `define-key-s'."
  (define-key-s keymap key-defs))

;; * add-exec-path
(defun add-exec-path (path)
  (interactive "Dexec-path: ")
  (setenv "PATH" (concat path ";" (getenv "PATH")))
  (push path exec-path))

;; * add-watchwords
(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

;; * pretty symbol
(defvar *unicode-symbol*
  (mkht
   left-arrow 8592
   up-arrow 8593
   right-arrow 8594
   down-arrow 8595
   double-vertical-bar #X2551
   equal #X003d
   not-equal #X2260
   identical #X2261
   not-identical #X2262
   less-than #X003c
   greater-than #X003e
   less-than-or-equal-to #X2264
   greater-than-or-equal-to #X2265
   logical-and #X2227
   logical-or #X2228
   logical-neg #X00AC
   'nil #X2205
   dagger #X2020
   double-dagger #X2021
   horizontal-ellipsis #X2026
   reference-mark #X203B
   double-exclamation #X203C
   prime #X2032
   double-prime #X2033
   for-all #X2200
   there-exists #X2203
   element-of #X2208
   square-root #X221A
   squared #X00B2
   cubed #X00B3
   lambda #X03BB
   alpha #X03B1
   beta #X03B2
   gamma #X03B3
   delta #X03B4
   ))

(defun substitute-patterns-with-unicode (patterns)
  ""
  (mapcar
   (lambda (x)
     (font-lock-add-keywords
      nil `((,(car x)
             (0 (progn
                  (compose-region (match-beginning 1) (match-end 1)
                                  ;; ,(decode-char 'ucs (cdr (assoc (cdr x) *unicode-symbol*)))
                                  ,(decode-char 'ucs (gethash (cdr x) *unicode-symbol*))
                                  'decompose-region)
                  nil))))))
   patterns))

;; ** lisp symbol
(defun lisp-symbol ()
  (interactive)
  (substitute-patterns-with-unicode
   (to-alist '("(?\\(lambda\\>\\)" lambda
               ;; "\\<\\(lambda\\)\\>" lambda
               "\\(;;\\ \\)" reference-mark
               ;; "\\(<-\\)" left-arrow
               ;; "\\(->\\)" right-arrow
               ;; "\\(==\\)" identical
               ;; "\\(/=\\)" not-identical
               ;; "\\(>=\\)" greater-than-or-equal-to
               ;; "\\(<=\\)" less-than-or-equal-to
               ;; "\\(\\.\\.\\)" horizontal-ellipsis
               ;; "\\(()\\)" 'nil
               ;; "\\(!!\\)" double-exclamation
               ))))

