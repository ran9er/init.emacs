;;;###autoload
(defun usage-font (&rest fonts)
  (let ((l (if (listp (car fonts)) (car fonts) fonts))
        (r "Monospace"))
    (while
        (and
         l
         (null
          (if (x-list-fonts (car l))
              (setq r (car l))
            nil)))
      (setq l (cdr l)))
    r))

;; (usage-font
;;  "Yahei Consolas"
;;  "Yahei Mono"
;;  "Microsoft Yahei"
;;  "宋体"
;;  "文泉驿等宽微米黑"
;;  "文泉驿点阵正黑"
;;  "文泉驿等宽正黑")

;;;###autoload
(defun set-my-font0 (&rest lst)
  (let* ((key '(:zh :en))
         (default '("Monospace" 12))
         (memif
          (lambda(l o)
            (let (r)
              (while (and l (null (if (memq (car l) o)(setq r (car l)) nil)))
                (setq l (cdr l)))
              r)))
         (key (funcall memif key lst))
         (lst (delq key lst))
         (len (length lst))
         (base-font (or (nth 0 lst) (nth 0 default)))
         (base-font-size (or (nth 1 lst) (nth 1 default)))
         (ext-font (nth 2 lst))
         (ext-font-size (nth 3 lst)))
    (cond
     ((<= len 2)
      (set-frame-font (format "Monospace-%s" base-font-size))
      (set-fontset-font (frame-parameter nil 'font)
                        'unicode `(,base-font . "unicode-bmp")))
     ((or (null key)(eq key :zh))
      (let* ((en-font (format "%s %s" base-font base-font-size))
             (zh-font (font-spec :family ext-font :size ext-font-size))
             (script '(greek cyrillic hangul kana han cjk-misc bopomofo symbol mathematical)))
        (set-face-attribute 'default nil :font en-font)
        (mapc
         (lambda (charset)
           (set-fontset-font (frame-parameter nil 'font) charset zh-font))
         script)))
     ((eq key :en)
      (set-frame-font (format "%s-%s" base-font base-font-size))
      ;; (set-frame-font (format "Monospace-%s" base-font-size))
      (set-fontset-font (frame-parameter nil 'font)
                        'latin `(,ext-font . "unicode-bmp"))))))


;;;###autoload
(defun set-font (script font)
  (set-fontset-font (frame-parameter nil 'font) script font))

;;;###autoload
(defun set-my-font (base-fonts
                    base-font-size
                    &optional
                    ext-fonts
                    ext-font-size)
  (if ext-fonts
      (let* ((en-font (format "%s %s" base-fonts base-font-size))
             (zh-font (font-spec :family ext-fonts :size ext-font-size))
             (script '(cyrillic hangul kana han cjk-misc bopomofo symbol mathematical)))
        (set-face-attribute 'default nil :font en-font)
        (mapc
         (lambda (charset)
           (set-fontset-font (frame-parameter nil 'font) charset zh-font))
         script))
    (set-frame-font (format "Monospace-%s" base-font-size))
    (set-fontset-font (frame-parameter nil 'font)
                      'unicode `(,base-fonts . "unicode-bmp"))))

;;;###autoload
(defun set-my-bf-mode (font size)
  "set buffer face mode"
  (set-face-attribute
   'variable-pitch nil
   :font (format "%s-%s" font size)
   :fontset "fontset-standard"))

;;;###autoload
(defun set-my-ui-font (height font)
  "set mode-line & header-line"
  (custom-set-faces
   `(mode-line ((t (:height ,height :family ,font))))
   `(header-line ((t (:height ,height :family ,font))))))

;;;###autoload
(defun my-buffer-face-mode(&optional ls)
  (buffer-face-mode)
  (make-local-variable 'line-spacing)
  (setq line-spacing (or ls 4)))

;;;###autoload
(defun custom-buffer-face-mode()
  (interactive)
  (if (and (boundp 'buffer-face-mode) buffer-face-mode)
      nil
    (my-buffer-face-mode)))
