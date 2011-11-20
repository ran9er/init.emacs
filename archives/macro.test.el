;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
(defun x ()               `(message "%s"  (buffer-name)))
(defun xx ()              `(message "%s" ,(buffer-name)))
(defmacro xxx ()          `(message "%s"  (buffer-name)))
(defmacro xxxx ()         `(message "%s" ,(buffer-name)))

(defun bar (arg)             (message "%d %d"  arg  arg))
(defun barr (arg)           `(message "%d %d" ,arg ,arg))

(defmacro foo (arg)         `(message "%d %d" ,arg ,arg))
(defmacro fooo (arg)   (list 'message "%d %d"  arg  arg))
                          (x)
                         (xx)
                        (xxx)
                       (xxxx)
(let ((b 1))  (bar (incf b)))
(let ((b 1)) (barr (incf b)))
(let ((f 1))  (foo (incf f)))
(let ((f 1)) (fooo (incf f)))





(macroexpand '(foo (incf x)))

(list 'a 'b 'c)
('a 'b 'c)
`(a b c )


