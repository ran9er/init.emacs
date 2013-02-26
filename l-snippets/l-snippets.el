;; * init
(add-to-list 'debug-ignored-errors "^Beginning of buffer$")
(add-to-list 'debug-ignored-errors "^End of buffer$")
;; (add-to-list 'debug-ignored-errors "^End of file during parsing$")

;; ** face
(defgroup l-snippets nil
  "Visual insertion of l-snippets."
  :group 'abbrev
  :group 'convenience)

(defface l-snippets-editable-face
  '((((background dark)) (:background "steel blue"))
    (((background light)) (:background "light cyan")))
  "*Face used for editable text in l-snippets."
  :group 'l-snippets)

(defface l-snippets-auto-face
  '((((background dark)) (:underline "steel blue"))
    (((background light)) (:underline "light cyan")))
  "*Face used for automatically updating text in l-snippets."
  :group 'l-snippets)

(defface l-snippets-auto-form-face
  '((default (:inherit 'l-snippets-auto-face)))
  "*Face used for text in l-snippets that is re-evaluated on input."
  :group 'l-snippets)

(defface l-snippets-tail-face
  '((((background dark)) (:underline "dim gray"))
    (((background light)) (:underline "dim gray")))
  "*Face used for text in l-snippets that is re-evaluated on input."
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

;; * customize
(defvar l-snippets-dir
  (file-name-directory
   (or load-file-name
       buffer-file-name)))

(setq l-snippets-repo (expand-file-name "snippets" l-snippets-dir))
(setq l-snippets-extension (expand-file-name "extensions" l-snippets-dir))
(defvar l-snippets-index-file "_index")

(defvar l-snippets-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [?\t] 'l-snippets-next-field)
    (define-key keymap "\M-n" 'l-snippets-next-field)
    (define-key keymap "\M-p" 'l-snippets-previous-field)
    (define-key keymap [remap move-beginning-of-line] 'l-snippets-beginning-of-field)
    (define-key keymap [remap move-end-of-line] 'l-snippets-end-of-field)
    (define-key keymap (kbd "C-x =") (lambda()(interactive)(setq current-overlay (l-snippets-get-overlay))))
    keymap)
  "*Keymap used for l-nippets input fields.")

(defvar l-snippets-syntax-meta
  '(head "\\$" open "{" close "}" path-separator "%" id "[[:digit:]]+"))

(defvar l-snippets-syntax-delimiter
  '((":" l-snippets-action-prompt)
    ("\\$" (lambda(s p o)(eval (read s)))))
  "string position overlay lst")

(defun l-snippets-action-prompt (s p o)
  (insert s)
  (l-snippets-move-overlay o p (point)))

(setq l-snippets-roles
 `(
   control
   ((role . control)
    (evaporate . t)
    (keymap . ,l-snippets-keymap)
    (insert-in-front-hooks l-snippets-move-primary)
    (face . l-snippets-auto-face))
   primary
   ((role . primary)
    (id . nil)
    (offset . 0)
    (tail . nil)
    (prompt . nil)
    (group . nil)
    (mirrors . nil)
    (modification-hooks l-snippets-this-overlay l-snippets-update-mirror)
    (insert-in-front-hooks l-snippets-this-overlay l-snippets-update-mirror)
    ;; (insert-behind-hooks l-snippets-this-overlay)
    (local-map . ,l-snippets-keymap)
    (face . l-snippets-editable-face))
   tail
   ((role . tail)
    (primary . nil)
    (priority . 1)
    (face . l-snippets-tail-face)
    ;; (modification-hooks l-snippets-this-overlay)
    (local-map . ,l-snippets-keymap)
    (insert-in-front-hooks l-snippets-this-overlay l-snippets-delete-prompt l-snippets-move-primary l-snippets-update-mirror))
   mirror
   ((role . mirror)
    (primary . nil)
    (face . l-snippets-auto-face))
   ))

;; * func
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

(defun l-snippets-temp-name (make-temp-name (format "--%s-"(buffer-name))))

;; * init
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

(defun l-snippets-move-overlay (o b e &optional nontail)
  (let* ((primary (l-snippets-get-primary o))
         (tail (l-snippets-get-tail o))
         (te (if nontail e (1+ e))))
    (move-overlay tail e te)
    (move-overlay primary b e)))

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

(defun l-snippets-overlay-update-text (ov text)
  (let ((beg (overlay-start ov)))
    (save-excursion
      (goto-char beg)
      ;; (delete-char (- (overlay-end ov) beg))
      (delete-region beg (overlay-end ov))
      (insert text)
      (move-overlay ov beg (point)))))

(defun l-snippets-overlay-appoint (role &optional b e &rest properties)
  (let* ((inhibit-modification-hooks t)
         (b (or b (point)))
         (e (or e b))
         (void (if (> e (point-max))
                   (save-excursion
                     (insert (make-string (- e (point-max)) ?\ )))))
         (rl (plist-get l-snippets-roles role))
         (ov (if rl (l-snippets-make-overlay b e))))
    (run-hooks 'l-snippets-before-overlay-appoint-hook)
    (mapc
     (lambda(x)
       (overlay-put ov (car x)
                    (or (plist-get properties (car x))
                        (cdr x))))
     rl)
    (run-hooks 'l-snippets-after-overlay-appoint-hook)
    ov))

(defun l-snippets-overlay-release (ov)
  (mapc
   (lambda(x)
     (l-snippets-delete-overlay x))
   (overlay-get ov 'mirrors))
  (l-snippets-delete-overlay (overlay-get ov 'tail))
  (l-snippets-delete-overlay ov))

(defun l-snippets-get-primary(ov)
  (if (eq 'primary (overlay-get ov 'role))
      ov
    (overlay-get ov 'primary)))

(defun l-snippets-get-tail(ov)
  (if (eq 'tail (overlay-get ov 'role))
      ov
    (overlay-get ov 'tail)))

;; ** hooks
(defun l-snippets-update-mirror (overlay after-p beg end &optional length)
  (if after-p
      (let* ((inhibit-modification-hooks t)
             (overlay (l-snippets-get-primary overlay))
             (text (buffer-substring-no-properties
                    (overlay-start overlay)
                    (overlay-end overlay)))
             (mirrors (overlay-get overlay 'mirrors)))
        (mapc
         (lambda(x)
           (l-snippets-overlay-update-text x text))
         mirrors))))

(defun l-snippets-delete-prompt (overlay after-p beg end &optional length)
  (let ((ov (l-snippets-get-primary overlay)))
   (if (null (overlay-get ov 'prompt))
      nil
    (delete-region (overlay-start ov)(overlay-end ov))
    (overlay-put ov 'prompt nil))))
(defun l-snippets-move-primary (overlay after-p beg end &optional length)
  (let ((own (overlay-get overlay 'primary))
        (pos (1- (overlay-end overlay))))
    (if after-p
        (l-snippets-move-overlay overlay (overlay-start own) pos))))

;; (defun l-snippets-move-tail (overlay after-p beg end &optional length)
;;   (if after-p
;;       (let* ((tail (overlay-get overlay 'tail))
;;              (beg (overlay-end overlay))
;;              (end (1+ beg)))
;;         (if (eq beg (overlay-start tail))
;;             nil
;;           (move-overlay tail beg end)))))

(defun l-snippets-this-overlay (overlay after-p beg end &optional length)
  (if after-p
      nil
    (condition-case nil
        (throw 'current-overlay overlay)
      (error))))


;; * debug
(defun what-overlays (&optional p)
  (interactive)
  (print
   (let ((pt (or p (point))))
     (cons (list 'position pt '& 'column (current-column))
           (mapcar
            (lambda(x) (remove-if
                    nil
                    (list
                     (overlay-start x)(overlay-end x)
                     (overlay-get x 'role))))
            (overlays-in pt (1+ pt)))))))

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
(defun l-snippets-get-overlay()
  (interactive)
  (catch 'current-overlay
    (save-excursion
      (insert " ")
      (delete-char -1))))

(defun l-snippets-goto-field (&optional n)
  (interactive)
  (let* ((n (or n 1))
         (o (l-snippets-get-primary (l-snippets-get-overlay)))
         (lst (cdr (assoc (overlay-get o 'group) l-snippets-instance)))
         (oo (cdr (nth (- (- (length lst) (length (member (rassoc o lst) lst)))n)lst))))
    (overlay-put o 'offset (- (overlay-end o)(point)))
    (goto-char (- (overlay-end oo)(overlay-get oo 'offset)))))

(defun l-snippets-previous-field ()
  (interactive)
  (l-snippets-goto-field -1))

(defun l-snippets-next-field ()
  (interactive)
  (l-snippets-goto-field 1))

(defun l-snippets-beginning-of-field ()
  (interactive)
  (let ((o (l-snippets-get-primary (l-snippets-get-overlay))))
    (goto-char (overlay-start o))))

(defun l-snippets-end-of-field ()
  (interactive)
  (let ((o (l-snippets-get-primary (l-snippets-get-overlay))))
    (goto-char (overlay-end o))))

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
  (setq
   l-snippets-index
   (let ((mtime (lambda(x)(nth 5 (file-attributes x))))
         (l-snippets-index-file
          (expand-file-name
           l-snippets-index-file
           l-snippets-repo)))
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
     (l-snippets-read-index l-snippets-index-file))))

(l-snippets-update-index)

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
(defun l-snippets-find-close-paren (x y &optional back)
  (let ((count 1)
        (re-search (if back 're-search-backward 're-search-forward))
        open close)
    (save-excursion
      (funcall re-search x)
      (setq open (match-beginning 0))
      (while (> count 0)
        (funcall re-search
                 (format "\\(%s\\)\\|\\(%s\\)" x y))
        (or
         (if (match-beginning 1)
             (setq count (1+ count)))
         (if (match-end 2)
             (setq count (1- count))))
        (setq close (cons (match-beginning 2) (match-end 2)))))
    close))

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
        (goto-char (point-min))
        (setq end (car (l-snippets-find-close-paren
                        (plist-get l-snippets-syntax-meta 'open)
                        (plist-get l-snippets-syntax-meta 'close))))
        (setq result (buffer-substring-no-properties beg end)))))
    (cons id result)))

(defun l-snippets-split-str (str &optional sep)
  (let* ((sep (or sep l-snippets-token-regexp-delimiter))
         (lst l-snippets-syntax-delimiter)
         (elt (l-snippets-make-lst (length lst)))
         end result)
    (with-temp-buffer
      (insert str)
      (setq end (point-max))
      (goto-char end)
      (while (re-search-backward sep nil t)
        (mapcar
         (lambda(x)
           (if (match-end x)
               (let ((m
                      (buffer-substring-no-properties
                       (match-end x)
                       end))
                     (n (nth 1 (nth (1- x) lst))))
                 (setq result
                       (cons
                        (cons n m)
                        result)
                       end
                       (match-beginning x)))))
         elt))
      result)))

(defun l-snippets-prase-token (str)
  (let* ((lst (l-snippets-fetch-str str))
         (id (car lst))
         (act (l-snippets-split-str (cdr lst))))
    (apply
     'list
     id
     (mapcar
      (lambda(x) (cons (car x) (cdr x)))
      act))))

(defun l-snippets-gen-token (file &optional regexp)
  (let ((regexp l-snippets-token-regexp-open)
        beg mid end result)
    (with-temp-buffer
      (when (file-readable-p file)
        (insert-file-contents file nil nil nil t)
        (setq end (point-max))
        (goto-char end)
        (while (re-search-backward regexp nil t)
          (setq
           beg
           (match-beginning 0)
           mid
           (if (match-beginning 1)
               (cdr (l-snippets-find-close-paren
                     (plist-get l-snippets-syntax-meta 'open)
                     (plist-get l-snippets-syntax-meta 'close)))
             (match-end 2))
           result
           (cons
            (l-snippets-prase-token
             (buffer-substring-no-properties beg mid))
            (cons
             (buffer-substring-no-properties mid end)
             result))
           end
           beg))
        (if (eq beg (point-min))
            result
          (setq
           result
           (cons
            (buffer-substring-no-properties (point-min) beg)
            result)))))))

;; * insert
(defun l-snippets-insert-str (str)
  (let* ((ov (l-snippets-get-overlay))
         st end)
    (if ov (progn
             (setq ov (l-snippets-get-primary ov)
                   st (overlay-start ov)
                   end (overlay-end ov))
             (l-snippets-move-overlay ov st end t)))
    (if l-snippets-enable-indent
        (let ((str (split-string str " \t\n" t))(l 0))
          (while str
            (if (> l 0)
                (forward-line))
            (insert (car str))
            (indent-according-to-mode)
            (setq str (cdr str)
                  l (1+ l))))
      (insert str))
    (if ov (l-snippets-move-overlay ov st end))))

(defun l-snippets-insert (snippet-name)
  (let* ((snippet (l-snippets-get-snippet snippet-name))
         (top (eq (current-indentation) 0))
         (lst 'l-snippets-instance)
         (n (make-temp-name ""))
         l)
    (if top
        "...clear" )
    (mapc
     (lambda (x)
       (if (stringp x)
           (l-snippets-insert-str x)
         (let* ((id (car x))
                (args (cdr x))
                (p (point))
                role o)
           (cond
            ((assoc id l) (setq role 'mirror
                                o (l-snippets-overlay-appoint role p p 'group n))
             (let ((prim (cdr (assoc id l)))(p (point)))
               (l-snippets-overlay-push-to prim o 'mirrors)
               (overlay-put o 'primary prim)
               (insert
                (buffer-substring-no-properties
                 (overlay-start prim)
                 (overlay-end prim)))
               (move-overlay o p (point))))
            ;; endpos
            ((eq id 0)
             )
            (id (setq role 'primary
                      o (l-snippets-overlay-appoint role p p 'group n))
                (overlay-put
                 o
                 'tail
                 (l-snippets-overlay-appoint
                  'tail
                  (overlay-end o)
                  (1+ (overlay-end o))
                  'primary o))
                (overlay-put o 'id (list snippet-name n id))
                (setq l (cons (cons id o) l)))
            (t (setq role 'void)))
           (mapc
            (lambda(x)(funcall (car x) (cdr x) p o))
            args)
           )))
     snippet)
    (mapc (lambda(x)(overlay-put (cdr x) 'prompt t)) l)
    (set lst (cons (cons n l)(eval lst)))
    (goto-char (overlay-end (cdr (car (last l)))))))

;; * interface
(defun l-snippets-fetch-word ()
  (interactive)
  (save-excursion
    (buffer-substring-no-properties
     (progn
       (skip-chars-backward " \t\n")
       (point))
     (progn
       (backward-sexp)
       (point)))))

(defun l-snippets-clear-word (str &optional back)
  (let*  ((str (split-string str "[ \t\n]" t)))
    (if back (setq str (reverse str)))
    (save-excursion
      (while str
        (if back
            (skip-chars-backward " \t\n")
          (skip-chars-forward " \t\n"))
        (let* ((p1 (point))
               (p2 (if back (1+ (search-backward-regexp "[ \t\n]"))
                     (1- (search-forward-regexp "[ \t\n]"))))
               (s (buffer-substring-no-properties p1 p2)))
          (if (equal (car str) s)
              (delete-region p1 p2)))
        (setq str (cdr str))))))

(defun l-snippets-clear-region (snippet)
  (let* ((s (l-snippets-get-snippet snippet))
         (head (car s))
         (tail (car (last s))))
    (if (stringp head)
        (l-snippets-clear-word head t))
    (if (stringp tail)
        (l-snippets-clear-word tail))))

(defun l-snippets-match (str)
  (mapconcat 'identity
             (list
              (symbol-name major-mode)
              str)
             (plist-get
              l-snippets-syntax-meta
              'path-separator)))

(defvar l-snippets-match-strategy 'l-snippets-match)

;;;###autoload
(defun l-snippets-expand ()
  (interactive)
  (let ((sp (funcall l-snippets-match-strategy
              (l-snippets-fetch-word))))
    (l-snippets-clear-region sp)
    (l-snippets-insert sp)
    ))

(let ((f (directory-files l-snippets-extension t ".*\\.el\\'")))
  (if f (mapc (lambda(x)(load x)) f)))

;; End of file during parsing
;(setq l-snippets-instance nil)
;(setq l-snippets-enable-indent nil)
;(l-snippets-insert "python-mode%%class")
