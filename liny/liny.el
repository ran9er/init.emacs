;; * init
(add-to-list 'debug-ignored-errors "^Beginning of buffer$")
(add-to-list 'debug-ignored-errors "^End of buffer$")
;; (add-to-list 'debug-ignored-errors "^End of file during parsing$")

;; ** face
(defgroup liny nil
  "LINY Is Not Yasnippet."
  :group 'abbrev
  :group 'convenience)

(defface liny-editable-face
  '((((background dark)) (:background "dim gray"))
    (((background light)) (:background "dim gray")))
  "*Face used for editable text in LINY."
  :group 'liny)

(defface liny-active-face
  '((((background dark)) (:background "steel blue"))
    (((background light)) (:background "light cyan")))
  "*Face used for active text in LINY."
  :group 'liny)

(defface liny-auto-face
  '((((background dark)) (:underline "steel blue"))
    (((background light)) (:underline "light cyan")))
  "*Face used for automatically updating text in LINY."
  :group 'liny)

(defface liny-auto-form-face
  '((default (:inherit 'liny-auto-face)))
  "*Face used for text in LINY that is re-evaluated on input."
  :group 'liny)

(defface liny-tail-face
  '((((background dark)) (:underline "dim gray"))
    (((background light)) (:underline "dim gray")))
  "*Face used for tail field in LINY."
  :group 'liny)

;; * customize
(defvar liny-dir
  (file-name-directory
   (or load-file-name
       buffer-file-name)))

(setq liny-repo (expand-file-name "snippets" liny-dir))
(setq liny-extension (expand-file-name "extensions" liny-dir))

(defvar liny-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [?\t] 'liny-next-field)
    (define-key keymap "\M-n" 'liny-next-field)
    (define-key keymap "\M-p" 'liny-previous-field)
    (define-key keymap [remap move-beginning-of-line] 'liny-beginning-of-field)
    (define-key keymap [remap move-end-of-line] 'liny-end-of-field)
    (define-key keymap (kbd "C-x =") (lambda()(interactive)(setq current-overlay (liny-get-overlay))))
    (define-key keymap (kbd "C-x -") 'liny-clear-instance)
    keymap)
  "*Keymap used for l-nippets input fields.")

(defun liny-custom-syntax (&rest args)
  (let ((args (liny-to-alist args)))
    (setq liny-custom-meta
          (copy-sequence liny-syntax-meta)
          liny-custom-delimiter
          (copy-alist liny-syntax-delimiter))
    (mapc
     (lambda(x)
       (if (symbolp (car x))
           (plist-put liny-custom-meta (car x)(cdr x))
         (setq liny-custom-delimiter
               (cons
                x
                (remove
                 (rassq (cdr x) liny-custom-delimiter)
                 (remove
                  (assoc (car x) liny-custom-delimiter)
                  liny-custom-delimiter))))))
     args)))

(defvar liny-syntax-meta
  '(head "\\$" open "{" close "}" id "[[:digit:]]+"
         path-separator "%" file-separator "<------"))

(defvar liny-custom-meta nil)

(defvar liny-syntax-delimiter
  '(("\\(:\\)" . liny-action-prompt)
    ("\\(\\$\\)(" . (lambda(s p o)(eval (read s)))))
  "string position overlay")

(defvar liny-custom-delimiter nil)

(defun liny-gen-regexp (str &rest tags)
  (apply 'format
         str
         (mapcar
          (lambda(x)
            (plist-get
             (or liny-custom-meta
                 liny-syntax-meta) x))
          tags)))

(defvar liny-token-tags
  '((leader "\\(%s%s\\)\\|\\(%s%s\\)" head open head id)
    (sep    "\\(%s\\)\\|\\(%s\\)" id open)
    (head "%s" head)
    (open "%s" open)
    (close "%s" close)
    (id "%s" id)
    (file-separator "%s" file-separator)))

(defun liny-token-regexp (tag)
  (apply 'liny-gen-regexp (cdr (assoc tag liny-token-tags))))

(defun liny-token-delimiter ()
  (or liny-custom-delimiter liny-syntax-delimiter))

(defun liny-token-regexp-delimiter()
  (mapconcat
   'identity
   (mapcar
    (lambda(x)
      (format "%s" (car x)))
    (liny-token-delimiter))
   "\\|"))

(defun liny-action-prompt (s p o)
  (insert s)
  (liny-move-overlay o p (point))
  (overlay-put o 'prompt t))

(defvar liny-roles
  `(end
    ((role . end)
     ;; (evaporate . t)
     (previous . nil)
     (face . liny-tail-face)
     (insert-in-front-hooks liny-this-overlay)
     (first . nil)
     (snippet-ready . nil)
     (keymap . ,liny-keymap))
    primary
    ((role . primary)
     (id . nil)
     (ready . nil)
     (offset . 0)
     (tail . nil)
     (prompt . nil)
     (previous . nil)
     (next . nil)
     (mirrors . nil)
     (modification-hooks liny-this-overlay
                         liny-update-mirror)
     (insert-in-front-hooks liny-this-overlay
                            liny-update-mirror)
     (local-map . ,liny-keymap)
     (face . liny-editable-face))
    tail
    ((role . tail)
     (primary . nil)
     (priority . 1)
     ;; (face . liny-tail-face) ;; debug
     (local-map . ,liny-keymap)
     (insert-in-front-hooks liny-this-overlay
                            liny-delete-prompt
                            liny-move-primary
                            liny-update-mirror))
    mirror
    ((role . mirror)
     (primary . nil)
     (modification-hooks liny-this-overlay)
     (local-map . ,liny-keymap) ;; debug
     (face . liny-auto-face))
    _null
    ((role . _null)
     (modification-hooks liny-null-ov-hook)
     (insert-in-front-hooks liny-null-ov-hook))
    ))

;; * func
(defun liny-to-alist (lst)
  (if lst
      (cons
       (cons (nth 0 lst) (nth 1 lst))
       (liny-to-alist (nthcdr 2 lst)))))

(defun liny-make-lst (n)
  (let* ((i n)(x nil))
    (while (> i 0)
      (setq x (cons i x))
      (setq i (1- i)))
    x))

(defun liny-temp-name (make-temp-name (format "--%s-"(buffer-name))))

;; * init
(defvar liny-enable-overlays-pool nil)

(defvar liny-overlays-pool nil)

(defvar liny-enable-indent t)
(make-local-variable 'liny-enable-indent)

(defvar liny-cache
  (make-hash-table :test 'equal))

(defun liny-clear-cache()
  (interactive)
  (clrhash liny-cache))

;; * overlay

(defun liny-make-overlay (b e)
  (let* ((p 'liny-overlays-pool)
         (q (eval p))
         (ov (car q)))
    (if liny-enable-overlays-pool
        (setq ov (make-overlay b e))
      (if ov
          (progn
            (set p (cdr-safe q))
            (move-overlay ov b e))
        (setq ov (make-overlay b e))))
    ov))

(defun liny-move-overlay (o b e &optional nontail)
  (let* ((primary (liny-get-primary o))
         (tail (liny-get-tail o))
         (te (if nontail e (1+ e))))
    (move-overlay tail e te)
    (move-overlay primary b e)))

(defun liny-delete-overlay (ov)
  (let ((p 'liny-overlays-pool)
        (ov (or ov liny-null-ov)))
    (mapc
     (lambda(x)
       (overlay-put ov (car x) nil))
     (plist-get liny-roles (overlay-get ov 'role)))
    (delete-overlay ov)
    (if liny-enable-overlays-pool
        (set p (cons ov (eval p))))
    nil))


(defun liny-overlay-get-text (o)
  "liny-overlay-get-text is writen by ran9er"
  (buffer-substring-no-properties
   (overlay-start o)
   (overlay-end o)))

(defun liny-overlay-update-text (ov text)
  (let ((beg (overlay-start ov)))
    (save-excursion
      (goto-char beg)
      ;; (delete-char (- (overlay-end ov) beg))
      (delete-region beg (overlay-end ov))
      (insert text)
      (move-overlay ov beg (point)))))

(defun liny-overlay-appoint (role &optional b e &rest properties)
  (let* ((inhibit-modification-hooks t)
         (b (or b (point)))
         (e (or e b))
         (void (if (> e (point-max))
                   (save-excursion
                     (insert (make-string (- e (point-max)) ?\ )))))
         (rl (plist-get liny-roles role))
         (ov (if rl (liny-make-overlay b e))))
    (run-hooks 'liny-before-overlay-appoint-hook)
    (mapc
     (lambda(x)
       (overlay-put ov (car x)
                    (or (plist-get properties (car x))
                        (cdr x))))
     rl)
    (if (eq role 'primary)
        (overlay-put
         ov
         'tail
         (liny-overlay-appoint
          'tail
          (overlay-end ov)
          (1+ (overlay-end ov))
          'primary ov)))
    (run-hooks 'liny-after-overlay-appoint-hook)
    ov))

(defun liny-overlay-release (ov)
  (mapc
   (lambda(x)
     (liny-delete-overlay x))
   (overlay-get ov 'mirrors))
  (liny-delete-overlay (overlay-get ov 'tail))
  (liny-delete-overlay ov))

(defun liny-clear-instance (ov)
  "liny-clear-instance "
  (interactive)
  (let* ((cur (liny-get-primary ov))
         head)
    (while
        (prog1 (overlay-get cur 'previous)
          (setq head cur))
      (setq cur (overlay-get cur 'previous)))
    (while
        (prog1 (overlay-get head 'next)
          (setq cur head
                head (overlay-get head 'next))
          (liny-overlay-release cur)))))

(defun liny-get-prev (ov id)
  (if (overlayp ov)
      (if (eq id (nth 1 (overlay-get ov 'id)))
          ov
        (liny-get-prev (overlay-get ov 'previous) id))))

(defun liny-get-primary(ov)
  (if (eq 'primary (overlay-get ov 'role))
      ov
    (overlay-get ov 'primary)))

(defun liny-get-tail(ov)
  (if (eq 'tail (overlay-get ov 'role))
      ov
    (overlay-get ov 'tail)))

(defun liny-get-first (ov)
  "liny-get-first is writen by ran9er"
  (let ((o (liny-get-primary ov)))
    (while (overlay-get o 'previous)
      (setq o (overlay-get o 'previous)))
    o))

(defun liny-get-last (ov)
  "liny-get-last is writen by ran9er"
  (let ((o (if (eq (overlay-get ov 'role) 'end)
               o (liny-get-primary ov))))
    (while (overlay-get o 'next)
      (setq o (overlay-get o 'next)))
    o))


;; ** struction
(defun liny-overlay-push-to (to from &optional p)
  (let ((p (or p 'group)))
    (overlay-put to p (cons from (overlay-get to p)))))

(defun liny-overlay-link (front beg &optional end)
  (let ((end (or end beg)))
    (overlay-put (overlay-get front 'next) 'previous end)
    (overlay-put end 'next (overlay-get front 'next))
    (overlay-put front 'next beg)
    (overlay-put beg 'previous front)))

;; (defun liny-overlay-setprev (to from &optional p)
;;   (let ((p (or p 'link)))
;;    (overlay-put to p (cons from (cdr (overlay-get to p))))))

;; (defun liny-overlay-setnext (to from &optional p)
;;   (let ((p (or p 'link)))
;;    (overlay-put to p (cons (car (overlay-get to p)) from))))

(defun liny-overlay-append-hooks (ov prop &rest hooks)
  (overlay-put ov prop
               (append (overlay-get ov prop)
                       hooks)))

(defun liny-primary-append-hooks (ov &rest hooks)
  "liny-primary-append-hooks is writen by ran9er"
  (apply 'liny-overlay-append-hooks ov 'modification-hooks hooks)
  (apply 'liny-overlay-append-hooks ov 'insert-in-front-hooks hooks)
  (if (overlay-get ov 'tail)
      (apply 'liny-overlay-append-hooks
             (overlay-get ov 'tail) 'insert-in-front-hooks hooks)))


(defun liny-clone-primary (ov beg)
  (let* ((ids (overlay-get ov 'id))
         (o (car
             (liny-insert-field
              'primary
              ids
              (cdr
               (assoc
                (nth 1 ids)
                (liny-get-snippet
                 (nth 0 ids))))
              beg))))
    (liny-overlay-link ov o)
    ;; (mapc
    ;;  (lambda(x)
    ;;    (goto-char (overlay-end x))
    ;;    (liny-overlay-push-to
    ;;     o
    ;;     (car (liny-insert-field 'mirror ids nil (point) o))
    ;;     'mirrors)
    ;;    (liny-ex-template
    ;;     x
    ;;     (liny-gen-token
    ;;      (overlay-get x 'dynamic-template))))
    ;;  (overlay-get ov 'mirrors))
    (goto-char (overlay-end o))
    o))

;; ** hooks
(defun liny-update-mirror (overlay after-p beg end &optional length)
  (if after-p
      (let* ((inhibit-modification-hooks t)
             (overlay (liny-get-primary overlay))
             (text (buffer-substring-no-properties
                    (overlay-start overlay)
                    (overlay-end overlay)))
             (mirrors (overlay-get overlay 'mirrors)))
        (mapc
         (lambda(x)
           (liny-overlay-update-text x text))
         mirrors))))

(defun liny-delete-prompt (overlay after-p beg end &optional length)
  (let ((ov (liny-get-primary overlay)))
    (if (overlay-get ov 'ready)
        (if after-p
            (if (null (overlay-get ov 'prompt))
                nil
              (delete-region (overlay-start ov)(overlay-end ov))
              (overlay-put ov 'prompt nil))))))

(defun liny-move-primary (overlay after-p beg end &optional length)
  (let ((own (overlay-get overlay 'primary))
        (pos (1- (overlay-end overlay))))
    (if after-p
        (liny-move-overlay overlay (overlay-start own) pos))))

;; (defun liny-display-tail (overlay after-p beg end &optional length)
;;   (let* ((pri (overlay-get overlay 'primary))
;;          (len (- (overlay-end pri)(overlay-start pri)))
;;          (tail (overlay-get pri 'tail)))
;;     (if (zerop len)
;;         (overlay-put tail 'face liny-tail-face)
;;       (overlay-put tail 'face nil))))

;; (defun liny-move-tail (overlay after-p beg end &optional length)
;;   (if after-p
;;       (let* ((tail (overlay-get overlay 'tail))
;;              (beg (overlay-end overlay))
;;              (end (1+ beg)))
;;         (if (eq beg (overlay-start tail))
;;             nil
;;           (move-overlay tail beg end)))))

(defun liny-this-overlay (overlay after-p beg end &optional length)
  (if after-p
      nil
    (condition-case nil
        (throw 'current-overlay overlay)
      (error))))

(defun liny-throw ()
  "liny-virtual-edit is writen by ran9er"
  (condition-case nil
      (throw 'liny-throw t)
    (error)))

(defun liny-null-ov-hook (overlay after-p beg end &optional length)
  "liny-null-ov-hook is writen by ran9er"
  (if after-p
      (delete-overlay overlay)))

;; * keymap
(defun liny-get-overlay()
  (interactive)
  (catch 'current-overlay
    (save-excursion
      (insert " ")
      (delete-char -1))))

(defun liny-goto-field (p-or-n)
  (interactive)
  (let* ((o (liny-get-primary (liny-get-overlay)))
         (oo (overlay-get o p-or-n)))
    (if (eq (overlay-get oo 'role) 'end)
        (if (y-or-n-p "finish this snippet?")
            (progn
              (goto-char (overlay-end oo))
              (liny-clear-instance o)))
      (overlay-put o 'offset (- (overlay-end o)(point)))
      (overlay-put o 'face 'liny-editable-face)
      (goto-char (- (overlay-end oo)(overlay-get oo 'offset)))
      (overlay-put oo 'face 'liny-active-face))))

(defun liny-previous-field ()
  (interactive)
  (liny-goto-field 'previous))

(defun liny-next-field ()
  (interactive)
  (liny-goto-field 'next))

(defun liny-beginning-of-field ()
  (interactive)
  (let ((o (liny-get-primary (liny-get-overlay))))
    (goto-char (overlay-start o))))

(defun liny-end-of-field ()
  (interactive)
  (let ((o (liny-get-primary (liny-get-overlay))))
    (goto-char (overlay-end o))))

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

;; * index
(defun liny-read-index (idx)
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

(defun liny-update-index (file str &optional force)
  (interactive)
  (setq
   liny-index
   (let ((mtime (lambda(x)(nth 5 (file-attributes x))))
         (liny-index-file
          (expand-file-name file liny-repo)))
     (if (or force
             (time-less-p
              (or (funcall mtime liny-index-file)
                  '(0 0 0))
              (funcall mtime liny-repo)))
         (with-temp-file
             (let ((enable-local-variables nil)
                   (find-file-hook nil))
               liny-index-file)
           (insert str)
           (message (format "Save %s." liny-index-file))))
     (liny-read-index liny-index-file))))

(defun liny-snippet-exist-p (snippet)
  (member snippet liny-index))

(liny-update-index "_index"
                   (pp-to-string
                    (directory-files liny-repo nil "^[^_].*\\'")))

(defun liny-get-snippet (snippet)
  (or
   (gethash snippet liny-cache)
   (if (liny-snippet-exist-p snippet)
       (puthash snippet
                (liny-get-token
                 (expand-file-name snippet liny-repo))
                liny-cache))))

;; * prase
(defun liny-find-close-paren (x y &optional back)
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

(defun liny-search-str (str &optional eof)
  "with-temp-buffer"
  (save-excursion
    (goto-char (point-min))
    (let ((start
           (re-search-forward
            (format "%s%s.*\n"
                    (liny-token-regexp 'file-separator)
                    str)
            nil t))
          (end
           (if eof (point-max)
             (and
              (re-search-forward
               (liny-token-regexp 'file-separator) nil t)
              (1- (match-beginning 0))))))
      (if (and start end)
          (buffer-substring-no-properties start end)))))

(defun liny-delete-tail-space (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-max))
    (buffer-substring-no-properties
     (point-min)
     (progn
       (skip-chars-backward " \t\n")
       (point)))))

(defun liny-fetch-str (str sep)
  (let (id result beg end)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (re-search-forward (liny-token-regexp 'head) nil t)
      (re-search-forward sep nil t)
      (cond
       ((match-end 1)
        (setq id (read (buffer-substring-no-properties
                        (match-beginning 1)(match-end 1)))
              result ""))
       ((match-end 2)
        (setq beg (match-end 2))
        (setq id (if (re-search-forward (liny-token-regexp 'id) nil t)
                     (read (buffer-substring-no-properties
                            (match-beginning 0)(match-end 0)))))
        (setq beg (match-end 0))
        (goto-char (point-min))
        (setq end (car (liny-find-close-paren
                        (liny-token-regexp 'open)
                        (liny-token-regexp 'close))))
        (setq result (buffer-substring-no-properties beg end)))
       (t
        (setq result str))))
    (cons id result)))

(defun liny-split-str (str delimiter elt)
  (let* (end result)
    (with-temp-buffer
      (insert str)
      (setq end (point-max))
      (goto-char end)
      (while (re-search-backward delimiter nil t)
        (mapcar
         (lambda(x)
           (if (match-end x)
               (let ((m
                      (buffer-substring-no-properties
                       (match-end x)
                       end))
                     (n (cdr (nth (1- x) (liny-token-delimiter)))))
                 (setq result
                       (cons
                        (cons n m)
                        result)
                       end
                       (match-beginning x)))))
         elt))
      result)))

(defun liny-prase-token (str sep delimiter elt)
  (let* ((lst (liny-fetch-str str sep))
         (id (car lst))
         (act (liny-split-str (cdr lst) delimiter elt)))
    (apply
     'list
     id
     (mapcar
      (lambda(x) (cons (car x) (cdr x)))
      act))))

(defun liny-gen-token (str &optional regexp sep delimiter elt)
  (let ((regexp (or regexp (liny-token-regexp 'leader)))
        (sep (or sep (liny-token-regexp 'sep)))
        (delimiter (or delimiter (liny-token-regexp-delimiter)))
        (elt (liny-make-lst (length (liny-token-delimiter))))
        beg mid prev result)
    (with-temp-buffer
      (insert str)
      (setq beg (point-min))
      (goto-char beg)
      (while (re-search-forward regexp nil t)
        (setq beg (match-beginning 0)
              mid (if (match-beginning 1)
                      (progn (goto-char (match-beginning 1))
                             (cdr (liny-find-close-paren
                                   (liny-token-regexp 'open)
                                   (liny-token-regexp 'close))))
                    (match-end 2)))
        (if prev
            (setq result
                  (cons (buffer-substring-no-properties (cdr prev) beg)
                        (cons (car prev)
                              result)))
          (setq result
                (cons (buffer-substring-no-properties (point-min) beg)
                      result)))
        (setq prev
              (cons
               (liny-prase-token
                (buffer-substring-no-properties beg mid) sep delimiter elt)
               mid))
        (goto-char mid))
      (setq result
            (cons
             (liny-delete-tail-space
              (buffer-substring-no-properties
               (cdr prev)(point-max)))
             (cons (car prev)
                   result))))
    (setq liny-custom-meta nil
          liny-custom-delimiter nil)
    (reverse result)))

(defun liny-get-token (file &optional regexp)
  (let (str env)
    (with-temp-buffer
      (when (file-readable-p file)
        (insert-file-contents file nil nil nil t)
        (setq
         env
         (liny-search-str "environment"))
        (if (> (length (replace-regexp-in-string "[ \t\n]" "" env)) 0)
            (eval (read env)))
        (setq
         str
         (liny-search-str "snippet" t))
        (liny-gen-token str regexp)))))

;; * insert
(defun liny-insert-str (str)
  (let* ((ov (liny-get-primary
              (or (liny-get-overlay)
                  liny-null-ov)))
         st end)
    (if ov (progn
             (setq st (overlay-start ov)
                   end (overlay-end ov))
             (liny-move-overlay ov st end t)))
    (if liny-enable-indent
        (let ((str (split-string str " \t\n" t))(l 0))
          (while str
            (if (> l 0)
                (forward-line))
            (insert (car str))
            (indent-according-to-mode)
            (setq str (cdr str)
                  l (1+ l))))
      (insert str))
    (if ov (liny-move-overlay ov st end))))

(defun liny-insert-field (role ids args pos &optional prev first last end)
  "liny-insert-field "
  (let* ((o (liny-overlay-appoint role pos pos))
         (id (nth 1 ids)))
    (cond
     ((eq role 'mirror)
      (let* ((prim (liny-get-prev prev id)))
        (liny-overlay-push-to prim o 'mirrors)
        (overlay-put o 'primary prim)
        (goto-char (overlay-end o))
        (if args nil
          (insert
           (buffer-substring-no-properties
            (overlay-start prim)
            (overlay-end prim)))
          (move-overlay o pos (point)))))
     ((eq role 'end)
      (overlay-put o 'id ids)
      ;; (overlay-put o 'previous prev)
      ;; (if prev (overlay-put prev 'next o))
      (overlay-put o 'first first)
      (setq end o))
     ((eq role 'primary)
      (overlay-put o 'id ids)
      (overlay-put o 'previous prev)
      (if prev (overlay-put prev 'next o))
      (setq prev o last o)
      (if (null first)(setq first o)))
     (t))
    (mapc
     (lambda(x)(funcall (car x) (cdr x) pos o))
     args)
    (list o prev first last end)))

(defun liny-insert (snippet-name &optional snippet-p)
  (let* ((snippet (if snippet-p snippet-name
                    (liny-get-snippet snippet-name)))
         (top (eq (current-indentation) 0))
         prev first last end)
    ;; (if top
    ;;     (liny-clear-instance))
    (mapc
     (lambda (x)
       (if (stringp x)
           (liny-insert-str x)
         (let* ((id (car x))
                (args (cdr x))
                (p (point))
                (ids (list snippet-name id))
                role o)
           (cond
            ((eq id 0)(setq role 'end))
            ((liny-get-prev prev id)
             (setq role 'mirror))
            (id (setq role 'primary))
            (t (setq role 'void)))
           (let  ((lst (liny-insert-field
                        role ids args p
                        prev first last end)))
             (setq ;; o (car lst)
              prev (nth 1 lst)
              first (nth 2 lst)
              last (nth 3 lst)
              end (nth 4 lst))))))
     snippet)
    ;; loop
    ;; (overlay-put first 'previous last)(overlay-put last 'next first)
    (cond
     (end
      (overlay-put last 'next end)
      (overlay-put end 'previous last)
      (setq last end)))
    (overlay-put first 'face 'liny-active-face)
    (while (progn
             (overlay-put prev 'ready t)
             (setq prev (overlay-get prev 'previous))))
    (goto-char (overlay-end first))
    (let ((f (overlay-get end 'snippet-ready)))
      (if f (funcall f end)))
    (cons first last)))

;; * interface
(defun liny-fetch-alias ()
  (interactive)
  (save-excursion
    (buffer-substring-no-properties
     (progn
       (skip-chars-backward " \t\n")
       (point))
     (progn
       (backward-sexp)
       (point)))))

(defun liny-match ()
  (mapconcat 'identity
             (list
              (symbol-name major-mode)
              (liny-fetch-alias))
             (plist-get
              liny-syntax-meta
              'path-separator)))

(defvar liny-match-strategy 'liny-match)

;;;###autoload
(defun liny-expand ()
  (interactive)
  (let ((spn (funcall liny-match-strategy)))
    (if (liny-get-snippet spn)
        (progn (kill-word -1)        ;; (liny-clear-region sp)
               (liny-insert spn)))))

;;;###autoload
(defun liny-expand-or-tab ()
  "liny-expand-or-tab is writen by ran9er"
  (interactive)
  (or (liny-expand)
      (indent-for-tab-command)))

(setq liny-null-ov (liny-overlay-appoint '_null 1))

;; load extension
(let ((f (directory-files liny-extension t ".*\\.el\\'")))
  (if f (mapc (lambda(x)(load x)) f)))
