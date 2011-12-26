;; -*- encoding: utf-8-unix; -*-
;; * cons-list
(defun cons-list (lst)
  "(cons-list '(1 2 3 4 5 6)) => ((1 . 2) (3 . 4) (5 . 6))"
  (if lst
      (cons
       (cons (car lst)(cadr lst))
       (cons-list (cddr lst)))))

(defun cons-list-l (lst)
  "(cons-list-l '(1 2 3 4 5 6)) => ((1 . 2) (3 . 4) (5 . 6))"
  (let* ((lst (if (eq (logand (length lst) 1) 1) `(,@lst nil) lst))
         (l (length lst))
         (new-list (cons (cons (nth (- l 2) lst)(nth (- l 1) lst)) nil))
         (cnt (1- (/ l 2))))
    (while (> cnt 0)
      (setq new-list (cons
                      (cons (nth (- l 4) lst)
                            (nth (- l 3) lst))
                      new-list)
            cnt (- cnt 1) l (- l 2)))
    new-list))

;; * zip-lists
(defun zip-lists (a b)
  "(zip-lists '(1 3 5) '(2 4 6)) => ((1 . 2) (3 . 4) (5 . 6))"
  (if (and a b)
      (cons
       (cons (car a)(car b))
       (zip-lists (cdr a)(cdr b)))))

;; * merge-lists
(defun merge-lists (&rest lists)
  "(merge-lists '(1 2) '(3 4) '(5 6)) => ((1 3 5) (2 4 6))"
  (let* ((l (length (car (last lists))))
         (m (1- (length lists)))
         (new-lists (zip-lists (car (last lists))(make-list l nil)))
         (lists (butlast lists)))
    (while (> m 0)
      (setq new-lists (zip-lists (car (last lists)) new-lists)
            lists (butlast lists)
            m (1- m)))
    new-lists))

;; * hash-table
(defun cons2hash (lst)
  (let ((h (make-hash-table :test 'equal)))
    (mapc (lambda(x)(puthash (car x)(cdr x) h)) lst)
    h))
(defun build-hash-table (&rest lst)
  (cons2hash (cons-list-l lst)))

(defun list-hash (hash-table &optional with-value)
  (let (lst)
    (if with-value
        (maphash (lambda(x y)(setq lst (cons (cons x y) lst))) hash-table)
      (maphash (lambda(x y)(setq lst (cons x lst))) hash-table))
    (reverse lst)))

;; * join-string
(defun join-string (lst s)
 "(mapconcat 'concat lst s)"
  (if (caddr lst)
      (concat (car lst) s (join-string (cdr lst) s))
    (concat (car lst) s (cadr lst))))

;; * concat symbol
(defun concat-symbol (&rest lst)
  (read (apply 'concat (mapcar (lambda(x)(if (symbolp x) (symbol-name x) x)) lst))))

;; * shell-command-symbol-to-string
(defmacro shell-command-symbol-to-string (&rest s)
  `(shell-command-to-string
    (apply 'concat (mapcar
     (lambda(x)(concat (symbol-name x) " "))
     ',s))))
(defalias 'ss 'shell-command-symbol-to-string)

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

;; * insert-time
(defun insert-time (&optional format)
  (interactive )
  (insert
   (format-time-string
    (or format "%Y-%m-%d {%u} %H:%M:%S")
    (current-time))))

;; * backward-kill-word-or-kill-region
(defun backward-kill-word-or-kill-region ()
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'backward-kill-word)))

;; * resize-horizontal-space
(defun resize-horizontal-space (&optional backward-only)
  (interactive "*P")
  (let (fwd-pos bwd-pos
        (orig-pos (point)))
    (setq
     fwd-pos (progn (skip-chars-forward " \t") (eolp))
     bwd-pos (progn (skip-chars-backward " \t") (bolp)))
    (goto-char orig-pos)
    (if (or fwd-pos bwd-pos)
        (delete-horizontal-space backward-only)
      (delete-horizontal-space backward-only)
      (insert " "))))

;; * parallel-edit
(defun insert-char-from-read(c)
  (cond
   ((eq c 13)
    (newline))
   ((eq c 127)
    (delete-backward-char 1))
   ((eq c 23)
    (backward-kill-word 1))
   (t
    (insert-char c 1))))
(defun mirror-region (src psn mkr-lst)
  "mirror region in mkr-lst, with str, and goto psn"
  (let ((str (buffer-substring-no-properties (car src)(cdr src))))
    (mapcar
     (lambda(x)
       (delete-region (car x)(1- (cdr x)))
       (goto-char (car x))
       (insert str))
     mkr-lst))
  (goto-char psn))
(defun parallel-edit (position-list &optional prt)
  (interactive)
  (let* ((p (or prt 0))
         (start-position (point-marker))
         end-position y x
         (end-marker (progn (forward-char (1+ p))(point-marker)))
         (marker-list (mapcar (lambda (x)
                                (cons
                                 (progn (goto-char x)(point-marker))
                                 (progn (forward-char (1+ p))(point-marker))))
                              position-list)))
    (goto-char start-position)
    (setq y nil)
    (while (null (eq (setq x (read-char "parallel-edit")) 13))
      (if y nil
        (delete-region start-position (1- end-marker)))
      (insert-char-from-read x)
      (setq end-position (1- end-marker)
            y t)
      (mirror-region (cons start-position end-position) end-position marker-list))))

;; * outside
(defmacro outside (o b s)
  "up list N level, append PRE ahead and SUF behind, backward M char"
  `(lambda(&optional n)
     (interactive "P")
     (let ((x (if n (prefix-numeric-value n) 1))
           beg end tmp delimiter)
       (if mark-active
           (setq delimiter ""
                 beg (region-beginning)
                 end (region-end))
         (setq delimiter ,s)
         (up-list x)
         (setq end (point))
         (setq beg (backward-list))
         (while (member (char-to-string (get-byte (1- beg)))
                        '("'" "`" "," "#" "@"))
           (setq beg (1- beg))))
       (setq tmp (buffer-substring-no-properties beg end))
       (delete-region beg end)
       (insert ,o)
       (backward-char ,b)
       (save-excursion
         (insert delimiter tmp)))))
;(def-key-s 0 "C-9" (outside "()" 1 " "))

;; * swap-point
(defun swap-point()
  (interactive)
  (if (or (null (boundp '*last-point*)) (null *last-point*))
      (progn (make-local-variable '*last-point*)
             (setq *last-point* (cons (point) (point))))
    (let ((p (point)))
      (if (eq p (cdr *last-point*))
          (progn (goto-char (car *last-point*))
                 (setq *last-point* (cons (cdr *last-point*)(car *last-point*))))
        (goto-char (cdr *last-point*))
        (setq *last-point* (cons p (cdr *last-point*)))))))

;; * temp file
(defun find-temp (&optional suffix)
  (interactive "sExtension: ")
  (let ((suf (if (and suffix (null (string= suffix "")))
                 (concat "." suffix))))
    (find-file
     (concat
      (make-temp-name
       (expand-file-name
        (format-time-string "%Y%m%d%H%M%S-" (current-time))
        work-dir))
      suf))
    (run-hooks 'find-temp-hook)))
(defun write-temp (filename &optional confirm)
  (interactive
   (list (if buffer-file-name
             (read-file-name "Write file: "
                             nil nil nil nil)
           (read-file-name "Write file: " default-directory
                           (expand-file-name
                            (file-name-nondirectory (buffer-name))
                            default-directory)
                           nil nil))
         (not current-prefix-arg)))
  (let ((fnm buffer-file-name))
    (write-file filename confirm)
    (if (file-exists-p fnm)
        (delete-file fnm))))
(add-hook 'find-temp-hook (lambda ()
                            (yank)))

;; * temp func
(defvar temp-func-list
  '((let ((cnt 0)(acc nil))
      (mapc (lambda(x) (setq acc (concat acc "\n"
                    (number-to-string (setq cnt (1+ cnt)))
                    ": " (prin1-to-string  x ))))
            (butlast temp-func-list))
      acc)))
(defun temp-func-add (&optional beg end)
  (interactive "r")
  (let* (b e 
           (x (if mark-active (read (buffer-substring-no-properties beg end))
                (up-list)(setq e (point))
                (backward-list)(setq b (point))
                (forward-list)
                (read (buffer-substring-no-properties b e)))))
    (if (null (equal x (car temp-func-list)))
        (push x temp-func-list)))
  (deactivate-mark))
(defun temp-func-call (&optional n)
  (interactive "p")
  (message
   (pp-to-string
    (let ((func (if (eq n 0)
                    (car (last temp-func-list))
                  (nth (1- n) temp-func-list))))
      (if (functionp func)
          (funcall func)
        (eval func))))))

;; * add-watchwords
(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

;; * pretty symbol
(defun unicode-symbol (name)
  "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
 or GREATER-THAN into an actual Unicode character code. "
  (decode-char 'ucs (case name
                      (left-arrow 8592)
                      (up-arrow 8593)
                      (right-arrow 8594)
                      (down-arrow 8595)
                      (double-vertical-bar #X2551)
                      (equal #X003d)
                      (not-equal #X2260)
                      (identical #X2261)
                      (not-identical #X2262)
                      (less-than #X003c)
                      (greater-than #X003e)
                      (less-than-or-equal-to #X2264)
                      (greater-than-or-equal-to #X2265)
                      (logical-and #X2227)
                      (logical-or #X2228)
                      (logical-neg #X00AC)
                      ('nil #X2205)
                      (dagger #X2020)
                      (double-dagger #X2021)
                      (horizontal-ellipsis #X2026)
                      (reference-mark #X203B)
                      (double-exclamation #X203C)
                      (prime #X2032)
                      (double-prime #X2033)
                      (for-all #X2200)
                      (there-exists #X2203)
                      (element-of #X2208)
                      (square-root #X221A)
                      (squared #X00B2)
                      (cubed #X00B3)
                      (lambda #X03BB)
                      (alpha #X03B1)
                      (beta #X03B2)
                      (gamma #X03B3)
                      (delta #X03B4))))
(defun substitute-pattern-with-unicode (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN with the
     Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
  (font-lock-add-keywords
   nil `((,pattern
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(unicode-symbol symbol)
                                    'decompose-region)
                    nil))))))
(defun substitute-patterns-with-unicode (patterns)
  "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
  (mapcar #'(lambda (x)
              (substitute-pattern-with-unicode (car x)
                                               (cdr x)))
          patterns))
;; ** lisp symbol
(defun lisp-symbol ()
  (interactive)
  (substitute-patterns-with-unicode
   (cons-list '("(?\\(lambda\\>\\)" lambda
                ;; "\\<\\(lambda\\)\\>" lambda
                "\\(;;\\ \\)" reference-mark
                "\\(<-\\)" left-arrow
                "\\(->\\)" right-arrow
                ;; "\\(==\\)" identical
                ;; "\\(/=\\)" not-identical
                "\\(>=\\)" greater-than-or-equal-to
                "\\(<=\\)" less-than-or-equal-to
                ;; "\\(\\.\\.\\)" horizontal-ellipsis
                ))))

;; * add-exec-path
(defun add-exec-path (path)
  (interactive "Dexec-path: ")
  (setenv "PATH" (concat path ";" (getenv "PATH")))
  (push path exec-path))

;; * load-once
(defmacro load-once (&rest s)
  (let* ((name (file-name-sans-extension
                (file-name-nondirectory
                 (or load-file-name (buffer-file-name)))))
         (a (concat-symbol "*load-once--" name "*")))
    `(if (boundp ',a)
         nil
       ,@s
       (setq ,a t))))

;; * test
(defun mklst (n)
  "创建大小为 n 的字符串列表"
  (let* ((i n)(x nil))
    (while (> i 0)
      (setq x (cons (number-to-string i) x))
      (setq i (1- i)))
    x))

(defun eval-buffer-time ()
  ""
  (interactive)
  (let ((tm (float-time)))
    (eval-buffer)
    (message (number-to-string (- (float-time) tm)))))

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
