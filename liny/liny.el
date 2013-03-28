;; * init
(add-to-list 'debug-ignored-errors "^Beginning of buffer$")
(add-to-list 'debug-ignored-errors "^End of buffer$")
;; (add-to-list 'debug-ignored-errors "^End of file during parsing$")

;; ** face
(defun adjust-color (color percentage)
  (let ((p (* 65535 (/ percentage 100.0))))
    (apply
     (lambda(r g b)
       (format "#%02x%02x%02x" r g b))
     (mapcar
      (lambda(x)
        (let ((v (+ x p)))
          (/ (cond
              ((> v 65535) 65535)
              ((< v 0) 0)
              (t v))
             257.0)))
      (color-values color)))))

(defgroup liny nil
  "LINY Is Not Yasnippet."
  :group 'abbrev
  :group 'convenience)

(defvar liny-base-color '("steel blue" "light cyan" "dim gray"))

(defface liny-active-face
  `((((background dark)) (:underline ,(nth 0 liny-base-color)))
    (((background light)) (:underline ,(nth 1 liny-base-color))))
  "*Face used for active text in LINY."
  :group 'liny)

(defface liny-editable-face
  `((((background dark))
     (:underline ,(adjust-color (nth 0 liny-base-color) -20)))
    (((background light))
     (:underline ,(adjust-color (nth 1 liny-base-color) -20))))
  "*Face used for editable text in LINY."
  :group 'liny)

(defface liny-auto-face
  `((((background dark))
     (:underline ,(nth 2 liny-base-color)))
    (((background light))
     (:underline ,(nth 2 liny-base-color))))
  "*Face used for automatically updating text in LINY."
  :group 'liny)

(defface liny-auto-form-face
  '((default (:inherit 'liny-auto-face)))
  "*Face used for text in LINY that is re-evaluated on input."
  :group 'liny)

(defface liny-tail-face
  `((((background dark))
     (:underline ,(adjust-color (nth 2 liny-base-color) -20)))
    (((background light))
     (:underline ,(adjust-color (nth 2 liny-base-color) -20))))
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
  '(head "\\$" open "{" close "}" id "[[:digit:]]+"  escape "[^\\\\]"
    path-separator "#" file-separator "<------"))

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
    (file-separator "%s" file-separator)
    (escape "%s" escape)))

(defun liny-token-regexp (tag)
  (apply 'liny-gen-regexp (cdr (assoc tag liny-token-tags))))

(defun liny-token-delimiter ()
  (or liny-custom-delimiter liny-syntax-delimiter))

(defun liny-token-regexp-delimiter()
  (mapconcat
   'identity
   (mapcar
    (lambda(x)
      (format "%s%s" (liny-token-regexp 'escape)(car x)))
    (liny-token-delimiter))
   "\\|"))

(defun liny-action-prompt (s p o)
  (insert s)
  (liny-move-overlay o p (point))
  (overlay-put o 'prompt t))

(defvar liny-roles
  `(origin
    ((role . origin)
     (id . nil)
     (insert-in-front-hooks liny-this-overlay)
     (origin . nil)
     (member . nil)
     (end . nil)
     (snippet-ready . nil)
     (snippet-exit . nil)
     (snippet-final . nil)
     (face . liny-tail-face) ;; debug
     (keymap . ,liny-keymap))
    final
    ((role . final)
     (origin . nil))
    end
    ((role . end)
     (id . nil)
     ;; (evaporate . t)
     (previous . nil)
     (face . liny-tail-face)
     (insert-in-front-hooks liny-this-overlay)
     (origin . nil)
     (keymap . ,liny-keymap))
    primary
    ((role . primary)
     (id . nil)
     (origin . nil)
     (ready . nil)
     (offset . 0)
     (tail . nil)
     (prompt . nil)
     (previous . nil)
     (next . nil)
     (mirrors . nil)
     (template . nil)
     (modification-hooks liny-this-overlay
                         liny-update-mirror)
     (insert-in-front-hooks liny-this-overlay
                            liny-update-mirror)
     (local-map . ,liny-keymap)
     (face . liny-editable-face))
    relay
    ((role . relay)
     (id . nil)
     (offset . 0)
     (previous . nil)
     (next . nil)
     (insert-in-front-hooks liny-this-overlay)
     (origin . nil)
     (keymap . ,liny-keymap))
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
     (origin . nil)
     (primary . nil)
     (template . nil)
     (modification-hooks liny-this-overlay)
     (local-map . ,liny-keymap) ;; debug
     (face . liny-auto-face))
    cur
    ((role . cur)
     (origin . nil))
    void
    ((role . void)
     (origin . nil))
    _null
    ((role . _null)
     (modification-hooks liny-null-ov-hook)
     (insert-in-front-hooks liny-null-ov-hook))
    ))

;; * func
(defun liny-run-hook (hook &rest args)
  "liny-run-hook is writen by ran9er"
  (if (symbolp hook)
      (if (boundp hook)
          (and (mapc (lambda(x)(apply x args)) (eval hook)) t))
    (and (mapc (lambda(x)(apply x args)) hook) t)))

(defun liny-to-alist (lst)
  (if lst
      (cons
       (cons (nth 0 lst) (nth 1 lst))
       (liny-to-alist (nthcdr 2 lst)))))

;; (defun liny-make-lst (n)
;;   (let* ((i n)(x nil))
;;     (while (> i 0)
;;       (setq x (cons i x))
;;       (setq i (1- i)))
;;     x))

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
         te)
    (if tail
        (progn
          (setq te (if nontail e (1+ e)))
          (move-overlay tail e te)))
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
    (if (memq role '(primary))
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
  ;; appoint to clear-instance
  ;; (mapc
  ;;  (lambda(x)
  ;;    (liny-delete-overlay x))
  ;;  (overlay-get ov 'mirrors))
  (liny-delete-overlay (overlay-get ov 'tail))
  (liny-delete-overlay ov)
  (liny-run-hook 'liny-overlay-release-hook ov))

(defun liny-clear-instance (&optional ov)
  (interactive)
  (let* ((prim (liny-get-primary
               (or ov (liny-get-overlay))))
         (origin (overlay-get prim 'origin)))
    (mapc (lambda(x)(liny-overlay-release x))(overlay-get origin 'member))
    (liny-overlay-release origin)))

;;;
(defun liny-ovl-get (o &rest l)
  "liny-get-ol is writen by ran9er"
  (if (> (length l) 1)
      (apply 'liny-ovl-get (overlay-get o (car l))(cdr l))
    (overlay-get o (car l))))

(defun liny-get-prev-id (ov id origin)
  (if (overlayp ov)
      (if (and
           (eq origin (overlay-get ov 'origin))
           (eq id (nth 1 (overlay-get ov 'id))))
          ov
        (liny-get-prev-id (overlay-get ov 'previous) id origin))))

(defun liny-get-primary(ov)
  (if (memq (overlay-get ov 'role) '(primary origin relay))
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

(defun liny-get-last (ov &optional role)
  "liny-get-last is writen by ran9er"
  (let* ((prop (or prop 'end))
         (o (if (eq (overlay-get ov 'role) role)
               o (liny-get-primary ov))))
    (while (overlay-get o 'next)
      (setq o (overlay-get o 'next)))
    o))

(defun liny-find-role (ov &optional r f-b)
  "liny-find-overlay is writen by ran9er"
  (let ((r (or r 'relay))
        (f-b (or f-b 'next)))
    (while (and (overlay-get ov f-b)
                (null (eq (overlay-get ov 'role) r)))
      (setq ov (overlay-get ov f-b)))
    ov))

;; ** struction
(defun liny-overlay-push-to (to p from)
  (overlay-put to p (cons from (overlay-get to p))))

(defun liny-overlay-link (front beg &optional end)
  (let ((end (or end beg))
        (n (overlay-get front 'next)))
    (if (null n) nil
      (overlay-put n 'previous end)
      (overlay-put end 'next n))
    (overlay-put front 'next beg)
    (overlay-put beg 'previous front)))

(defun liny-overlay-link-remove (ov)
  "liny-overlay-link-remove is writen by ran9er"
  (let ((p (overlay-get ov 'previous))
        (n (overlay-get ov 'next)))
    (overlay-put n 'previous p)
    (overlay-put p 'next n)
    (liny-overlay-release ov)))

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

(defun liny-clone-primary (ov beg &optional origin)
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
              beg
              origin))))
    (liny-overlay-link ov o)
    ;; (mapc
    ;;  (lambda(x)
    ;;    (goto-char (overlay-end x))
    ;;    (liny-overlay-push-to
    ;;     o 'mirrors
    ;;     (car (liny-insert-field 'mirror ids nil (point) nil o)))
    ;;    (liny-ex-template
    ;;     x
    ;;     (liny-gen-token
    ;;      (overlay-get x 'dynamic-template))))
    ;;  (overlay-get ov 'mirrors))
    (goto-char (overlay-end o))
    o))

;; ** hooks
(defun liny-update-mirror (overlay after-p beg end &optional length)
  (if (and after-p (overlay-buffer overlay))
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
    (if (and (overlayp ov) (overlay-get ov 'ready))
        (if after-p
            (if (null (overlay-get ov 'prompt))
                nil
              (delete-region (overlay-start ov)(overlay-end ov))
              (overlay-put ov 'prompt nil))))))

(defun liny-move-primary (overlay after-p beg end &optional length)
  (let ((own (overlay-get overlay 'primary))
        (pos (and (overlay-buffer overlay)(1- (overlay-end overlay)))))
    (if (and after-p own pos)
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

(defvar liny-goto-field-func 'liny-goto-field)


(defun liny-goto-field-really (ov &optional na)
  "liny-goto-field-1 is writen by ran9er"
  (if ov
      (cond
       ((eq (overlay-get ov 'role) 'end)
        (if (or na (y-or-n-p "finish this snippet?"))
            (progn
              (goto-char (overlay-end ov))
              (liny-run-hook (overlay-get
                              (overlay-get ov 'origin)
                              'snippet-exit) ov)
              (liny-clear-instance o))))
       (t
        (overlay-put o 'offset (- (overlay-end o)(point)))
        (overlay-put o 'face 'liny-editable-face)
        (if (or na (null (eq (overlay-get ov 'role) 'relay)))
            (goto-char (- (overlay-end ov)(overlay-get ov 'offset)))
          (liny-run-hook (overlay-get ov 'jump-relay-hooks) ov))
        (overlay-put ov 'face 'liny-active-face)))
    (or
     (liny-run-hook 'liny-goto-field-nil-hook)
     (message "End of world."))))

(defun liny-goto-field (p-or-n &optional na)
  (interactive)
  (let* ((o (liny-get-primary (liny-get-overlay)))
         (oo (overlay-get o p-or-n)))
    (liny-goto-field-really oo na)))

(defun liny-previous-field ()
  (interactive)
  (funcall liny-goto-field-func 'previous))

(defun liny-next-field ()
  (interactive)
  (funcall liny-goto-field-func 'next))

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
(defun liny-read-index (idxf)
  (let (var)
    (with-current-buffer
        (let ((enable-local-variables nil))
          (find-file-noselect idxf))
      (prog2
          (goto-char (point-min))
          (setq var
                (condition-case err
                    (read (current-buffer))
                  (error
                   nil)))
        (kill-buffer (current-buffer))))))

(defun liny-update-index-dir (file strs &optional force)
  "liny-update-index-dir is writen by ran9er"
  (let ((mtime (lambda(x)(nth 5 (file-attributes x))))
        (liny-index-file
         (expand-file-name file liny-repo))
        print-length print-level selective-display-ellipses)
    (if (or force
            (time-less-p
             (or (funcall mtime liny-index-file)
                 '(0 0 0))
             (funcall mtime liny-repo)))
        (with-temp-file
            (let ((enable-local-variables nil)
                  (find-file-hook nil))
              liny-index-file)
          (insert (pp-to-string (eval strs)))
          (message (format "Save %s." liny-index-file))))
    liny-index-file))

(defun liny-update-index (file strs &optional force)
  (interactive)
  (setq
   liny-index
   (liny-read-index
    (liny-update-index-dir file strs force))))

(defun liny-snippet-exist-p (snippet)
  (member snippet liny-index))

(liny-update-index "_index"
                   '(directory-files liny-repo nil "^[^_].*\\'"))

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
        (setq id (if (and (save-excursion
                            (re-search-forward (liny-token-regexp 'id) nil t))
                          (eq beg (match-beginning 0)))
                     (prog1
                         (read (buffer-substring-no-properties
                                (match-beginning 0)(match-end 0)))
                       (setq beg (match-end 0)))))
        (goto-char (point-min))
        (setq end (car (liny-find-close-paren
                        (liny-token-regexp 'open)
                        (liny-token-regexp 'close))))
        (setq result (buffer-substring-no-properties beg end)))
       (t
        (setq result str))))
    (cons id result)))

(defun liny-if-open-paren (&optional pos)
  (let* ((pos (or pos (point)))
         (c (buffer-substring-no-properties (1- pos) pos))
         (y (assoc c '(("{" . "}")("[" . "]")))))
    (if y
        (progn
          (backward-char)
          (goto-char (car (liny-find-close-paren
                           (regexp-quote c)
                           (regexp-quote (cdr y)))))))))

(defun liny-find-str (delimiter elt)
  (let ((i 1) key beg)
    (and
     (prog1
         (re-search-forward delimiter nil t)
       (while (or (and (null (match-end i))(<= i elt))
                  (progn (if (match-end i)
                             (setq
                              key (cdr (nth (1- i) (liny-token-delimiter)))
                              beg (match-end i)))
                         nil))
         (setq i (1+ i))))
     (cons key
           (apply
            'buffer-substring-no-properties
            (or
             (let ((x (liny-if-open-paren)))(if x (list (1+ beg) x)))
             (save-excursion
               (if (re-search-forward delimiter nil t)
                   (list beg (match-beginning 0))))
             (list beg (point-max))))))))

(defun liny-split-str (str delimiter elt)
  (let (result)
    (with-temp-buffer
      (insert " ")                      ;for escape
      (insert str)
      (goto-char (point-min))
      (while (let ((v (liny-find-str delimiter elt)))
               (if v (setq result (append result (list v)))))))
    result))

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
        (elt (length (liny-token-delimiter)))
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
(defun liny-insert-str (str &optional indent)
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
            (if indent
                (indent-according-to-mode))
            (setq str (cdr str)
                  l (1+ l))))
      (insert str))
    (if ov (liny-move-overlay ov st end))))

(defun liny-insert-field (role ids args pos &optional origin prev first last end)
  "liny-insert-field "
  (let* ((o (liny-overlay-appoint role pos pos))
         (id (nth 1 ids)))
    (cond
     ((memq role '(primary relay))
      (overlay-put o 'id ids)
      (overlay-put o 'previous prev)
      (if prev (overlay-put prev 'next o))
      (setq prev o last o)
      (if (null first)(setq first o))
      (if (eq role 'relay)
          (setq end o)))
     ((eq role 'mirror)
      (let* ((prim (liny-get-prev-id prev id origin)))
        (liny-overlay-push-to prim 'mirrors o)
        (overlay-put o 'primary prim)
        (goto-char (overlay-end o))
        (if args nil
          (insert
           (buffer-substring-no-properties
            (overlay-start prim)
            (overlay-end prim)))
          (move-overlay o pos (point)))))
     ((eq role 'origin)
      (overlay-put o 'id ids)
      (setq last o origin o)
      (overlay-put o 'end (or end last)))
     ((eq role 'end)
      (overlay-put o 'id ids)
      (overlay-put o 'first first)
      (setq end o))
     ((eq role 'cur)
      (setq first o)
      (liny-overlay-append-hooks
       origin 'snippet-ready 'liny-clear-instance))
     (t))
    (if (overlay-get o 'origin) nil
      (overlay-put o 'origin origin))
    (liny-overlay-push-to origin 'member o)
    (mapc
     (lambda(x)(funcall (car x) (cdr x) pos o))
     args)
    (list o origin prev first last end)))

(defun liny-insert-snippet (snippet-name &optional snippet-p relay
                                 origin prev first last end)
  (let* ((snippet (if snippet-p snippet-name
                    (liny-get-snippet snippet-name)))
         (first-line (line-number-at-pos))
         final)
    (if origin nil
      (let ((lst (liny-insert-field
                  'origin (list snippet-name) "" (point))))
        (setq   origin (nth 1 lst)
                prev   (nth 2 lst)
                first  (nth 3 lst)
                last   (nth 4 lst)
                end    (nth 5 lst))))
    (mapc
     (lambda (x)
       (if (stringp x)
           (liny-insert-str x (if (eq (line-number-at-pos) first) nil t))
         (let* ((id (car x))
                (args (cdr x))
                (p (point))
                (ids (if snippet-p nil (list snippet-name id)))
                role o)
           (cond
            ((eq id 0)(setq role (cond
                                  ((null prev) 'cur)
                                  (relay 'relay)
                                  (t 'end))))
            ((liny-get-prev-id prev id origin)
             (setq role 'mirror))
            (id (setq role 'primary))
            (t (setq role 'void)))
           (let ((lst (liny-insert-field
                       role ids args p
                       origin prev first last end)))
             (setq ;; o (car lst)
              prev (nth 2 lst)
              first (nth 3 lst)
              last (nth 4 lst)
              end (nth 5 lst))))))
     snippet)
    (setq final (liny-overlay-appoint 'final))
    (overlay-put origin 'snippet-final final)
    (liny-overlay-push-to origin 'member final)
    (while (progn
             (overlay-put (or prev liny-null-ov) 'ready t)
             (setq prev (overlay-get (or prev liny-null-ov) 'previous))))
    (if relay nil
      (overlay-put first 'face 'liny-active-face)
      (goto-char (overlay-end first)))
    (cond
     (end
      (overlay-put last 'next end)
      (overlay-put end 'previous last)
      (liny-run-hook (overlay-get origin 'snippet-ready) origin)))
    (list first last end origin final)))

(defun liny-insert (snippet-name &optional snippet-p relay
                                 origin prev first last end)
  (let* ((result (liny-insert-snippet snippet-name snippet-p relay
                                      origin prev first last end))
         (beg (overlay-start (nth 3 result)))
         (end (overlay-end (nth 4 result)))
         (setmkr (lambda(mk pt)
                   (let ((mkr mk))
                     (if (null (boundp mkr))
                         (progn
                           (make-local-variable mkr)
                           (set mkr (make-marker))))
                     (move-marker (eval mkr) pt)))))
    (funcall setmkr 'liny-expand-marker-beg beg)
    (funcall setmkr 'liny-expand-marker-end end)
    result))

;; * interface
(defvar liny-fetch-alias-func 'backward-sexp)

(defun liny-fetch-alias ()
  (interactive)
  (save-excursion
    (buffer-substring-no-properties
     (progn
       (skip-chars-backward " \t\n")
       (point))
     (progn
       (funcall liny-fetch-alias-func)
       (point)))))

(defvar liny-match-strategy 'liny-match)

(defun liny-match ()
  (mapconcat 'identity
             (list
              (symbol-name major-mode)
              (liny-fetch-alias))
             (plist-get
              liny-syntax-meta
              'path-separator)))

(defun liny-multi-snippets-select ()
  "liny-multi-snippets-select is writen by ran9er"
  (interactive)
  (if (> (length liny-match-snippets) 1)
      (progn
        (remove-overlays liny-expand-marker-beg liny-expand-marker-end)
        (delete-region liny-expand-marker-beg liny-expand-marker-end)
        (setq liny-match-snippets
              (append (cdr liny-match-snippets)(list (car liny-match-snippets))))
        (liny-insert (cdar liny-match-snippets)))))

;;;###autoload
(defun liny-expand ()
  (interactive)
  (let ((spn (funcall liny-match-strategy)))
    (if (liny-get-snippet spn)
        (progn
          (kill-word -1)        ;; (liny-clear-region sp)
          (liny-insert spn)))))

;;;###autoload
(defun liny-expand-maybe ()
  "liny-expand-or-tab is writen by ran9er"
  (interactive)
  (or (liny-expand)
      (let ((p (point)))
        (if (and (<= liny-expand-marker-beg p)
               (<= p liny-expand-marker-end))
          (liny-multi-snippets-select)))
      (call-interactively liny-expand-maybe-instead-command)))

(setq liny-null-ov (liny-overlay-appoint '_null 1))

;; load extension
(let ((f (directory-files liny-extension t ".*\\.el\\'")))
  (if f (mapc (lambda(x)(load x)) f)))

;; (remove-overlays (point-min)(point-max))
