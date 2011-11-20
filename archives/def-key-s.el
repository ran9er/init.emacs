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
              (t keymap))))
    (while key-defs
      (let* ((key (pop key-defs))
             (def (if (eq group nil)(pop key-defs) group)))
        (define-key
          map
          (eval `(kbd ,key))
          def)
        ))))

(defmacro def-k-s (km &rest kd)
  "(def-key-s map \"key\" def \"key\" def ...)
See also `define-key-s'."
;  (list 'define-key-s km `',kd))
  `(define-key-s ,km ',kd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun def-key-s (keymap &rest key-defs)
  "(def-key-s map \"key\" 'def \"key\" 'def ...)
See also `define-key-s'."
  (define-key-s keymap key-defs))
