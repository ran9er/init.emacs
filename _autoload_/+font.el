;; -*- encoding: utf-8-unix; -*-
;; File-name:    <+font.el>
;; Create:       <2012-02-17 20:23:36 ran9er>
;; Time-stamp:   <2012-02-17 20:33:51 Administrator>
;; Mail:         <2999am@gmail.com>

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
(defun set-my-font (base-fonts
                    base-font-size
                    &optional
                    ext-fonts
                    ext-font-size)
  (if ext-fonts
      (let* ((en-font (format "%s %s" base-fonts base-font-size))
             (zh-font (font-spec :family ext-fonts :size ext-font-size)))
        (set-face-attribute 'default nil :font en-font)
        (mapc
         (lambda (charset)
           (set-fontset-font (frame-parameter nil 'font) charset zh-font))
         '(kana han symbol cjk-misc bopomofo)))
    (set-default-font (format "Monospace-%s" base-font-size))
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
