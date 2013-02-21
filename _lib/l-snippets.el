;; [X] 可自定义语法
;; [ ] 可选字段，不定长字段(动态生成field；分隔符，镜像分隔符)
;; [X] 按需加载
;; [ ] 嵌套 snippets （按缩进）
;; [X] 自动缩进/原格式
;; [ ] 智能选择 snippets

(defun l-snippets-to-alist (lst)
  (if lst
      (cons
       (cons (nth 0 lst) (nth 1 lst))
       (l-snippets-to-alist (nthcdr 2 lst)))))

(defun l-snippets-make-lst (n)
  (let* ((i n)(x nil))
    (while (> i 0)
      (setq x (cons i x))
      (setq i (1- i)))
    x))


(setq l-snippets-repo (expand-file-name "sandbox/repo" *init-dir*))
;; * init
(add-to-list 'debug-ignored-errors "^Beginning of buffer$")
(add-to-list 'debug-ignored-errors "^End of buffer$")

(defgroup l-snippets nil
  "Visual insertion of tempo templates."
  :group 'abbrev
  :group 'convenience)

(defface l-snippets-editable-face
  '((((background dark)) (:background "steel blue"))
    (((background light)) (:background "light cyan")))
  "*Face used for editable text in tempo snippets."
  :group 'l-snippets)

(defface l-snippets-auto-face
  '((((background dark)) (:underline "steel blue"))
    (((background light)) (:underline "light cyan")))
  "*Face used for automatically updating text in tempo snippets."
  :group 'l-snippets)

(defface l-snippets-auto-form-face
  '((default (:inherit 'l-snippets-auto-face)))
  "*Face used for text in tempo snippets that is re-evaluated on input."
  :group 'l-snippets)

(defcustom l-snippets-interactive t
  "*Insert prompts for snippets.
If this variable is nil, snippets work just like ordinary l-templates with
l-interactive set to nil."
  :group 'l-snippets
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom l-snippets-grow-in-front nil
  "*If this is set, inserting text in front of a field will cause it to grow."
  :group 'l-snippets
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defvar l-snippets-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\M-n" 'l-snippets-next-field)
    (define-key keymap "\M-p" 'l-snippets-previous-field)
    keymap)
  "*Keymap used for l-nippets input fields.")

(defun l-snippets-temp-name (make-temp-name (format "--%s-"(buffer-name))))

(defvar l-snippets-syntax-meta
  '(head "\\$" open "{" close "}" path-separator "%%" id "[[:digit:]]+"))

(defvar l-snippets-syntax-delimiter
  '((":" (lambda(s p o)(insert s)(move-overlay o p (+ p (length s)))))
    ("\\$" (lambda(s p o)(eval (read s)))))
  "((delimiter(regexp) (lambda(string position overlay)(...)))(delimiter(regexp) (lambda(string position overlay)(...))) (...))")
;; (mapc
;;  (lambda(x) (funcall (nth 1 x) string overlay))
;;  l-snippets-syntax-delimiter)

(defmacro l-snippets-gen-regexp (str &rest tags)
  `(format
    ,str
    ,@(mapcar
       (lambda(x)
         (plist-get l-snippets-syntax-meta x))
       tags)))

(setq l-snippets-token-regexp-open
      (l-snippets-gen-regexp
       "\\(%s%s\\)\\|\\(%s%s\\)"
       head open head id))

(setq l-snippets-token-regexp-delimiter
      (mapconcat
       'identity
       (mapcar
        (lambda(x)
          (format "\\(%s\\)" (car x)))
        l-snippets-syntax-delimiter)
       "\\|"))

(defvar l-snippets-enable-overlays-pool nil)

(defvar l-snippets-overlays-pool nil)

(defvar l-snippets-instance nil)
(make-local-variable 'l-snippets-instance)

(defvar l-snippets-enable-indent nil)
(make-local-variable 'l-snippets-enable-indent)

(defvar l-snippets-cache
  (make-hash-table :test 'equal))

(defun l-snippets-clear-cache()
  (interactive)
  (clrhash l-snippets-cache))

(defvar l-snippets-repo
  (expand-file-name
   "repo/"
   (file-name-directory
    (or load-file-name
        buffer-file-name))))

(defvar l-snippets-index-file "_index")

;; * overlay

(defun l-snippets-make-overlay (b e)
  (let* ((p 'l-snippets-overlays-pool)
         (q (eval p))
         (ov (car q)))
    (if l-snippets-enable-overlays-pool
        (setq ov (make-overlay b e))
      (if ov
          (progn
            (set p (cdr-safe q))
            (move-overlay ov b e))
        (setq ov (make-overlay b e))))
    ov))

(defun l-snippets-delete-overlay (ov)
  (let ((p 'l-snippets-overlays-pool))
    (mapc
     (lambda(x)
       (overlay-put ov (car x) nil))
     (plist-get l-snippets-roles (overlay-get ov 'role)))
    (delete-overlay ov)
    (if l-snippets-enable-overlays-pool
        (set p (cons ov (eval p))))
    nil))

;(defvar
(defun l-snippets-field (overlay after? beg end &optional length)
  (let ((inhibit-modification-hooks t)
        (text (buffer-substring-no-properties
               (overlay-start overlay)
               (overlay-end overlay)))
        (mirrors (overlay-get overlay 'mirrors)))
    (save-excursion
      (mapc
       (lambda(x)
         (l-snippets-overlay-update-text x text))
       mirrors))))

(setq l-snippets-roles
 `(
   control
   ((role . control)
    (evaporate . t)
    (keymap . l-snippets-keymap))
   major
   ((role . major)
    (id . nil)
    (owner . nil)
    (group . l-snippets-instance)
    (mirrors . nil)
    ;; (more . nil)
    (modification-hooks  l-snippets-field)
    (insert-in-front-hooks l-snippets-field)
    (insert-behind-hooks l-snippets-field)
    (local-map . l-snippets-keymap)
    (face . tempo-snippets-editable-face))
   mirror
   ((role . mirror)
    (face . tempo-snippets-auto-face))
   test
   ((role . test)
    (id . nil)
    (owner . nil)
    (prompt . "this is a test string")
    (group . nil)
    (mirrors . nil)
    (more . nil)
    (modification-hooks  l-snippets-field)
    (insert-in-front-hooks l-snippets-field)
    (insert-behind-hooks l-snippets-field)
    (keymap . ,l-snippets-keymap)
    (face . tempo-snippets-editable-face))))

(defun l-snippets-overlay-update-text (ov text)
  (let ((beg (overlay-start ov)))
    (goto-char beg)
    ;; (delete-char (- (overlay-end ov) beg))
    (delete-region beg (overlay-end ov))
    (insert text)
    (move-overlay ov beg (point))))

(defun l-snippets-overlay-appoint (role &optional b  &rest properties)
  (let* ((inhibit-modification-hooks t)
         (b (or b (point)))
         (e b)
         (ov (l-snippets-make-overlay b e)))
    (run-hooks 'l-snippets-before-overlay-appoint-hook)
    (mapc
     (lambda(x)
       (overlay-put ov (car x)
                    (or (cdr x)
                        (plist-get properties (car x)))))
     (plist-get l-snippets-roles role))
    (run-hooks 'l-snippets-after-overlay-appoint-hook)
    ov))

(defun l-snippets-overlay-release (ov)
  (mapc
   (lambda(x)
     (l-snippets-delete-overlay x))
   (overlay-get ov 'mirrors))
  (l-snippets-delete-overlay ov))

(defun l-snippets-get-overlay (&optional rl position)
  (let* ((rl (or rl 'major))
         (pos (or position (point)))
         (olst (overlays-at pos))
         r)
    (while olst
      (if (eq (overlay-get (car olst) 'role) rl)
          (setq r (car olst)))
      (setq olst (cdr olst)))
    r))

;; ** stuction
(defun l-snippets-overlay-push-to (to from &optional p)
  (let ((p (or p 'group)))
   (overlay-put to p (cons from (overlay-get to p)))))

;; (defun l-snippets-overlay-setprev (to from &optional p)
;;   (let ((p (or p 'link)))
;;    (overlay-put to p (cons from (cdr (overlay-get to p))))))

;; (defun l-snippets-overlay-setnext (to from &optional p)
;;   (let ((p (or p 'link)))
;;    (overlay-put to p (cons (car (overlay-get to p)) from))))


;; * keymap
(defun l-snippets-next-field ()
  (interactive)
  (message "l-snippets-next-field"))
(defun l-snippets-previous-field ()
  (interactive)
  (message "l-snippets-previous-field"))

;; * index
(defun l-snippets-read-index (idx)
  (let (var)
    (with-current-buffer
        (let ((enable-local-variables nil))
          (find-file-noselect idx))
      (prog2
          (goto-char (point-min))
          (setq var
                (condition-case err
                    (read (current-buffer))
                  (error
                   nil)))
        (kill-buffer (current-buffer))))))

(defun l-snippets-update-index ()
  (interactive)
  (let ((mtime (lambda(x)(nth 5 (file-attributes x))))
        (l-snippets-index-file
         (expand-file-name
          l-snippets-index-file
          (file-name-directory
           l-snippets-repo))))
    (if (time-less-p
         (or (funcall mtime l-snippets-index-file)
             '(0 0 0))
         (funcall mtime l-snippets-repo))
        (with-temp-file
            (let ((enable-local-variables nil)
                  (find-file-hook nil))
              l-snippets-index-file)
          (insert (pp-to-string
                   (directory-files l-snippets-repo nil "^[^_].*\\'")))
          (message (format "Save %s." l-snippets-index-file))))
    (l-snippets-read-index l-snippets-index-file)))

(setq l-snippets-index
  (l-snippets-update-index))

(defun l-snippets-convert-to-snippet-name (abbrev)
  (mapconcat 'symbol-name
             (list major-mode abbrev)
             (plist-get
              l-snippets-syntax-meta
              'path-separator)))

(defun l-snippets-get-snippet (snippet)
  (or
   (gethash snippet l-snippets-cache)
   (if (member snippet l-snippets-index)
       (puthash snippet
                (l-snippets-gen-token
                 (expand-file-name snippet l-snippets-repo))
                l-snippets-cache)
     (message (format "%s is not define" snippet)))))

;; * prase
(defun l-snippets-find-close-paren (x y)
  (let ((count 1)
        open close)
    (save-excursion
      (re-search-forward x)
      (setq open (match-beginning 0))
      (while (> count 0)
        (re-search-forward
         (format "\\(%s\\)\\|\\(%s\\)" x y))
        (or
         (if (match-beginning 1)
             (setq count (1+ count)))
         (if (match-end 2)
             (setq count (1- count))))
        (setq close (match-end 2))))
    close))

(defun l-snippets-read-file (file regexp)
  (with-temp-buffer
    (when (file-readable-p file)
      (insert-file-contents file nil nil nil t)
      (goto-char (point-max))
      (list
       (buffer-substring-no-properties
        (point-min)
        (point-max))
       (let (a)
         (while (re-search-backward regexp nil t)
           (setq
            a
            (cons
             (cons
              (match-beginning 0)
              (if (match-beginning 1)
                  (l-snippets-find-close-paren
                   (plist-get l-snippets-syntax-meta 'open)
                   (plist-get l-snippets-syntax-meta 'close))
                (match-end 2)))
             a)))
         a)))))

(defun l-snippets-fetch-str (str)
  (let (id result beg end)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (re-search-forward (plist-get l-snippets-syntax-meta 'head) nil t)
      (re-search-forward
       (l-snippets-gen-regexp "\\(%s\\)\\|\\(%s\\)" id open) nil t)
      (cond
       ((match-end 1)
        (setq id (read (buffer-substring-no-properties
                        (match-beginning 1)(match-end 1)))
              result ""))
       ((match-end 2)
        (setq beg (match-end 2))
        (setq id (if (re-search-forward (plist-get l-snippets-syntax-meta 'id) nil t)
                     (read (buffer-substring-no-properties
                            (match-beginning 0)(match-end 0)))))
        (setq beg (match-end 0))
        (re-search-forward (plist-get l-snippets-syntax-meta 'close) nil t)
        (setq end (match-beginning 0))
        (setq result (buffer-substring-no-properties beg end)))))
    (cons id result)))

(defun l-snippets-split-str (str &optional sep)
  (let* ((sep (or sep l-snippets-token-regexp-delimiter))
         (lst l-snippets-syntax-delimiter)
         (elt (l-snippets-make-lst (length lst)))
         k result)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward sep nil t)
        (mapcar
         (lambda(x)
           (if (match-end x)
               (let ((m (cons
                         (match-beginning x)
                         (match-end x)))
                     (n (nth 1 (nth (1- x) lst))))
                 (setq result
                       (cons
                        (cons n m)
                        result)))))
         elt))
      (setq result (reverse result))
      (mapcar
       (lambda(x)
         (cons
          (car x)
          (buffer-substring-no-properties
           (cdr (cdr x))
           (or
            (car (cdr (car (cdr (member x result)))))
            (point-max)))))
       result))))

(defun l-snippets-prase-token (str)
  (let* ((lst (l-snippets-fetch-str str))
         (id (car lst))
         (act (l-snippets-split-str (cdr lst))))
    ;; (role (cond
    ;;        ((null id) nil)
    ;;        ((and act id) 'major)
    ;;        ((or act id) 'mirror)))
    (apply
     'list
     id
     (mapcar
      (lambda(x) (cons (car x) (cdr x)))
      act))))

(defun l-snippets-gen-token (file &optional regexp)
  (let* ((regexp l-snippets-token-regexp-open)
         (var (l-snippets-read-file file regexp))
         (str (nth 0 var))
         (len (length str))
         (lst (nth 1 var))
         (last (cdar (last lst)))
         (mkr '(1)))
    (mapc
     (lambda(x)
       (setq mkr (cons (car x) mkr)
             mkr (cons (car x) mkr)
             mkr (cons (cdr x) mkr)
             mkr (cons (cdr x) mkr)))
     lst)
    (if (> len last)
      (setq mkr (cons len mkr))
    (setq mkr (cdr mkr)))
    (setq mkr (reverse mkr)
          mkr (l-snippets-to-alist mkr))
    (remove
     nil
     (mapcar
      (lambda(x)
        (let* ((k (car x))(a (1- k))(b (1- (cdr x)))
               (s (substring-no-properties str a b)))
          (if (zerop (length s))
              nil
            (if (assoc k lst)
                (l-snippets-prase-token s)
              s))))
      mkr))))

;; * insert
(defun l-snippets-insert-str (str)
  (if l-snippets-enable-indent
      (insert
       (replace-regexp-in-string
        "\n\\([ \t]+\\)" "\t"
        str nil nil 1))
    (insert str)))

(defun l-snippets-insert (snippet)
  (let* ((snippet (l-snippets-get-snippet snippet))
         (top (eq (current-indentation) 0))
         (lst 'l-snippets-instance)
         (n (make-temp-name ""))
         l)
    (mapc
     (lambda (x)
       (if (stringp x)
           (l-snippets-insert-str x)
         (let ((id (car x))
               (args (cdr x))
               role)
           (setq role
                 (cond
                  ((assoc id l) 'mirror)
                  (t 'major)))
           ;; (set lst (cons (eval lst)))
           (if top
               "...clear" )
           (if id
               (let* ((p (point))
                      (o (l-snippets-overlay-appoint role)))
                 (cond
                  ((eq role 'major)
                   ;; (eval (overlay-get o 'group))
                   (setq l (cons (cons id o) l)))
                  ((eq role 'mirror)
                   (l-snippets-overlay-push-to
                    (cdr (assoc id l))
                    o 'mirror)))
                 (mapc
                  (lambda(x)(funcall (car x) (cdr x) p o))
                  args))
             (mapc
              (lambda(x)(funcall (car x) (cdr x) nil nil))
              args))
           )))
     snippet)
    (set lst (cons (cons n l)(eval lst)))))
;; End of file during parsing

;(setq l-snippets-instance nil)
;(l-snippets-insert "emacs-lisp-mode%%defun")
