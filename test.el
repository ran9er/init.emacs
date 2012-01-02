;; -*- encoding: utf-8-emacs-unix; -*-
;; background 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if t t;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
     '(default ((t (:stipple nil :background ((image :type jpeg :file "/Path/to/your/image.png") :origin display) :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 101 :width normal :family "misc-fixed")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq acc 0)

(-
(cdar *init-time*)
(apply '+ (mapcar 'cdr (cdr *init-time*)))
)

;; (hash-table-size *auto-hook-hash*)
;; (test-times 100
;;  (puthash (make-temp-name "") nil *auto-hook-hash*))

;; * export loaded times
(let ((var (read
            (concat
             "*load--"
             (or
              load-file-name
              (buffer-file-name))))))
  (eval `(defvar ,var 0))
  (eval `(setq ,var (1+ ,var))))
