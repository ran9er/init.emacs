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


(setq l-snippets-dir (expand-file-name "sandbox/l-snippets" *init-dir*))
(setq l-snippets-repo (expand-file-name "snippets" l-snippets-dir))
;; * init
(add-to-list 'debug-ignored-errors "^Beginning of buffer$")
(add-to-list 'debug-ignored-errors "^End of buffer$")
;; (add-to-list 'debug-ignored-errors "^End of file during parsing$")

;; ** face
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

(defface l-snippets-tail-face
  '((((background dark)) (:underline "white"))
    (((background light)) (:underline "white")))
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

;; * keymap
(defvar l-snippets-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\M-n" 'l-snippets-next-field)
    (define-key keymap "\M-p" 'l-snippets-previous-field)
    keymap)
  "*Keymap used for l-nippets input fields.")

;; * roles
(setq l-snippets-roles
 `(
   control
   ((role . control)
    (evaporate . t)
    (keymap . ,l-snippets-keymap)
    (insert-in-front-hooks l-snippets-move-major)
    (face . tempo-snippets-auto-face))
   major
   ((role . major)
    ;; (id . nil)
    (tail . nil)
    (prompt . t)
    (group . nil)
    (mirrors . nil)
    ;; (more . nil)
    (modification-hooks  l-snippets-update-mirror l-snippets-move-tail l-snippets-this-overlay)
    (insert-in-front-hooks l-snippets-update-mirror l-snippets-move-tail l-snippets-this-overlay)
    ;; (insert-behind-hooks l-snippets-this-overlay)
    (local-map . ,l-snippets-keymap)
    (face . tempo-snippets-editable-face))
   tail
   ((role . tail)
    (owner . nil)
    ;; (priority . 1)
    (face . tempo-snippets-auto-face)
    (insert-in-front-hooks l-snippets-move-major))
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
    (modification-hooks  l-snippets-update-mirror)
    (insert-in-front-hooks l-snippets-update-mirror)
    (insert-behind-hooks l-snippets-update-mirror)
    (keymap . ,l-snippets-keymap)
    (face . tempo-snippets-editable-face))))

(defvar l-snippets-syntax-meta
  '(head "\\$" open "{" close "}" path-separator "%%" id "[[:digit:]]+"))

(defvar l-snippets-syntax-delimiter
  '((":" (lambda(s p o l)
           (let (be)
             (insert s)
             (move-overlay o p (+ p (length s)))
             ;; (setq be (1+ (overlay-end o)))
             ;; (move-overlay (overlay-get o 'tail) be (1+ be))
             )
           ))
    ("\\$" (lambda(s p o l)(eval (read s)))))
  "string position overlay lst
((delimiter(regexp) (lambda(string position overlay)(...)))(delimiter(regexp) (lambda(string position overlay)(...))) (...))")

(defun l-snippets-temp-name (make-temp-name (format "--%s-"(buffer-name))))

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

(defun l-snippets-overlay-update-text (ov text)
  (let ((beg (overlay-start ov)))
    (goto-char beg)
    ;; (delete-char (- (overlay-end ov) beg))
    (delete-region beg (overlay-end ov))
    (insert text)
    (move-overlay ov beg (point))))

(defun l-snippets-overlay-appoint (role &optional b e &rest properties)
  (let* ((inhibit-modification-hooks t)
         (b (or b (point)))
         (e (or e b))
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

;; ** hooks
(defun l-snippets-update-mirror (overlay after-p beg end &optional length)
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

(defun l-snippets-move-major (overlay after-p beg end &optional length)
  (let ((own (overlay-get overlay 'owner)))
    (if after-p
        (let ((pos (1- (overlay-end overlay))))
          (move-overlay overlay pos (overlay-end overlay))
          (move-overlay own (overlay-start own) pos))
      (if (overlay-get own 'prompt)
          (progn
            (delete-region
             (overlay-start own)
             (overlay-end own))
            (overlay-put own 'prompt nil))))))

(defun l-snippets-move-tail (overlay after-p beg end &optional length)
  (let* ((tail (overlay-get overlay 'tail))
         (beg (overlay-end overlay))
         (end (1+ beg)))
    (if after-p
        (if (eq beg (overlay-start tail))
            nil
          (move-overlay tail beg end)))))

(defun l-snippets-this-overlay (overlay after-p beg end &optional length)
  (if after-p
      nil
      (condition-case err
          (throw 'current-overlay overlay)
        (error nil))))

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
  (catch 'current-overlay (insert "I hold not zhu!")))

(defun l-snippets-goto-field (&optional n)
  (interactive)
  (let* ((n (or n 1))
         (o (l-snippets-get-overlay))
         (lst (cdr (assoc (overlay-get o 'group) l-snippets-instance))))
    (goto-char (overlay-start (cdr (nth (- (- (length lst) (length (member (rassoc o lst) lst)))n)lst))))))

(defun l-snippets-previous-field ()
  (interactive)
  (l-snippets-goto-field -1))

(defun l-snippets-next-field ()
  (interactive)
  (l-snippets-goto-field 1))

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
  (if l-snippets-enable-indent
      (let ((str (split-string str "\n"))(l 0))
        (while str
          (if (> l 0)
              (forward-line))
          (insert (car str))
          (beginning-of-line)
          (delete-horizontal-space t)
          (indent-according-to-mode)
          (setq str (cdr str)
                l (1+ l))))
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
           (if top
               "...clear" )
           (if id
               (let* ((p (point))
                      (o (l-snippets-overlay-appoint role p (1+ p) 'group n)))
                 (cond
                  ((eq role 'major)
                   ;; (eval (overlay-get o 'group))
                   (overlay-put
                    o
                    'tail
                    (l-snippets-overlay-appoint
                     'tail
                     (overlay-end o)
                     (1+ (overlay-end o))
                     'owner o))
                   (setq l (cons (cons id o) l)))
                  ((eq role 'mirror)
                   (l-snippets-overlay-push-to
                    (cdr (assoc id l))
                    o 'mirrors)))
                 (mapc
                  (lambda(x)(funcall (car x) (cdr x) p o lst))
                  args))
             (mapc
              (lambda(x)(funcall (car x) (cdr x) nil nil lst))
              args))
           )))
     snippet)
    (set lst (cons (cons n l)(eval lst)))
    (goto-char (overlay-start (cdr (car (last l)))))))



;; * interface
(defun l-snippets-fetch-word ()
  )
(defun l-snippets-clear-region ()
  )
(defun l-snippets-match ()
  ;; defvar l-snippets-match-strategy
  ;; smart-match strict-match(by file-name)
  )

;;;###autoload
(defun l-snippets-expand ()
  (interactive)
  (l-snippets-insert
   (l-snippets-match
    (prog1
        (l-snippets-fetch-word)
      (l-snippets-clear-region)))))
;; End of file during parsing
;(setq l-snippets-instance nil)
;(setq l-snippets-enable-indent nil)
;(l-snippets-insert "python-mode%%class")
;(l-snippets-insert "emacs-lisp-mode%%defun")
