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

(defvar l-snippets-syntax-table
  '(head "\\$" open "{" close "}" delimiter ":" path-separator "%%" id "[[:digit:]]+"))

(defmacro l-snippets-gen-regexp (str &rest tags)
  `(format
    ,str
    ,@(mapcar
       (lambda(x)
         (plist-get l-snippets-syntax-table x))
       tags)))

(defvar l-snippets-token-regexp-open
  (l-snippets-gen-regexp
   "\\(%s%s\\)\\|\\(%s%s\\)"
   head open head id))

(defvar l-snippets-enable-overlays-pool nil)

(defvar l-snippets-overlays-pool nil)

(defvar l-snippets-cache
  (make-hash-table :test 'equal))

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
   field
   ((role . field)
    (id . nil)
    (owner . nil)
    ;; (prompt . nil)
    (link . (nil))
    (mirrors . nil)
    ;; (more . nil)
    (modification-hooks  l-snippets-field)
    (insert-in-front-hooks l-snippets-field)
    (insert-behind-hooks l-snippets-field)
    (local-map . l-snippets-keymap)
    (face . tempo-snippets-editable-face))
   mirror
   ((role . mirror)
    ;; (prompt . nil)
    (face . tempo-snippets-auto-face))
   test
   ((role . test)
    (id . nil)
    (owner . nil)
    (prompt . "this is a test string")
    (link . (nil))
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

(defun l-snippets-overlay-appoint (role b &optional prompt &rest properties)
  (let* ((inhibit-modification-hooks t)
         (prompt (or prompt
                     (cdr (assq 'prompt
                                (plist-get l-snippets-roles role)))))
         (e (prog1 (+ b (length prompt))
              (goto-char b)(insert prompt)))
         (ov (l-snippets-make-overlay b e)))
    (mapc
     (lambda(x)
       (overlay-put ov (car x)
                    (or (cdr x)
                        (plist-get properties (car x)))))
     (plist-get l-snippets-roles role))
    ov))

(defun l-snippets-overlay-release (ov)
  (mapc
   (lambda(x)
     (l-snippets-delete-overlay x))
   (overlay-get ov 'mirrors))
  (l-snippets-delete-overlay ov))

;; ** stuction
(defun l-snippets-overlay-push-to (to from &optional p)
  (let ((p (or p 'mirror)))
   (overlay-put to p (cons from (overlay-get to p)))))

(defun l-snippets-overlay-setprev (to from &optional p)
  (let ((p (or p 'link)))
   (overlay-put to p (cons from (cdr (overlay-get to p))))))

(defun l-snippets-overlay-setnext (to from &optional p)
  (let ((p (or p 'link)))
   (overlay-put to p (cons (car (overlay-get to p)) from))))


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
              l-snippets-syntax-table
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
                   (plist-get l-snippets-syntax-table 'open)
                   (plist-get l-snippets-syntax-table 'close))
                (match-end 2)))
             a)))
         a)))))

(defun split-str (sep str)
  (let (k result)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward sep nil t)
        (setq k (cons
                 (match-end 0)
                 (cons
                  (match-end 0)
                  (cons
                   (match-beginning 0)
                   (cons
                    (match-beginning 0)
                    k))))))
      (setq k (l-snippets-to-alist
               (l-snippets-to-alist
                (cdr (reverse (cons (point-max) k))))))
      (mapcar
       (lambda(x)
         (cons (buffer-substring-no-properties
                (car (car x)) (cdr (car x)))
               (buffer-substring-no-properties
                (car (cdr x)) (cdr (cdr x)))))
       k))))

(defun l-snippets-prase-token (str)
  (let (result)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (re-search-forward
       (plist-get l-snippets-syntax-table 'head) nil t)
      (let* ((p (point))
             (c (buffer-substring-no-properties p (1+ p))))
        (if (string-match "[[:digit:]]" c)
            ...
          ...))
      )
    result))

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
                ;; (l-snippets-prase-token s)
                (read (concat "[ " s " ]"))
              s))))
      mkr))))

;; * insert
(defun l-snippets-insert (snippet)
  (let* ((snippet (l-snippets-get-snippet snippet))
         idx lst)
    (mapc
     (lambda (x)
       (if (stringp x)
           (insert x)
         (let ((id (car x))
               (args (cdr x)))
           (if id
               (let ((o (apply 'l-snippets-overlay-appoint args)))
                 (setq lst (cons o lst))
                 (if (memq id idx)
                     (l-snippets-overlay-push-to "...")
                   (setq idx (cons id idx))))
             (insert (format "%s" x))))))
     snippet)))
