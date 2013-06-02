(setq skeleton-pair t)
(defvar skeleton-pair-cond-alist
  '(
    (?\( . ((t _ ")")))
    (?\{ . ((t n _ n "}")))
    (?\[ . (((char-bf ?=) n _ n "]")
            (t _ "]")))
    (?\" . ((t _ "\"")))
    (?,  . (((eolp) -1 " , ")
            (t)))
    (?=  . (((eolp) -1 " " "= ")
            (t)))
    ;; (?/  . (((bolp) "*" n  _  n "*/")
    ;;         (t _)))
    ;; (?.  . (((bolp) -1 "->")
    ;;         (t _)))
    ))

(defadvice skeleton-pair-insert-maybe (around condition activate)
  (let* ((skeleton-pair-alist skeleton-pair-alist)
         (key last-command-event)
         (cnd (cdr (assq key skeleton-pair-cond-alist))))
    (while
        (and
         cnd
         (null
          (if (eval (caar cnd))
              (setq skeleton-pair-alist (list (cons key (cdar cnd))))
            nil)))
      (setq cnd (cdr cnd)))
    ;; (message (format "%S" skeleton-pair-alist))
    ad-do-it))

(defun char-bf (x)
  (let ((x (if (listp x) x (list x))))
    (save-excursion
      (skip-chars-backward " \t")
      (memq (char-before (point)) x))))

(defun match-str-bf (regexp &optional back)
  "str-bf is writen by ran9er"
  (string-match
   regexp
   (save-excursion
     (skip-chars-backward " \t")
     (buffer-substring-no-properties
      (point)
      (progn (funcall (or back 'backward-sexp))(point))))))

;;;###autoload
(defun skeleton-pair-alist-update (&optional keymap)
  (interactive)
  (let ((keymap (or keymap (current-local-map))))
    (mapc ;; (local-set-key (char-to-string (cadr x)) 'skeleton-pair-insert-maybe)
     (eval
      `(lambda(x)
         (define-key ',keymap
           (eval `(kbd ,(char-to-string (car x))))
           ;; (char-to-string (cadr x))
           'skeleton-pair-insert-maybe)))
     skeleton-pair-cond-alist)))
;; (skeleton-pair-alist-update)

