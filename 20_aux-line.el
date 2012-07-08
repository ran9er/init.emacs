;; -*- encoding: utf-8-unix; -*-
;; File-name:    <20_indent-vline.el>
;; Create:       <2012-01-18 00:53:10 ran9er>
;; Time-stamp:   <2012-07-09 00:43:20 ran9er>
;; Mail:         <299am@gmail.com>

;; * indent-vline
(defun make-vline-xpm (width height color &optional lor)
  (let* ((w width)
         (h height)
         (s1 (concat "\"" (make-string w (string-to-char " ")) "\""))
         (s2 (cond
              ((eq lor 0)
               (concat "\"." (make-string (1- w) (string-to-char " ")) "\""))
              ((eq lor 1)
               (concat "\"" (make-string (1- w) (string-to-char " ")) ".\""))
              ((null lor)
               (concat "\"" (make-string (- (1- w)(/ (1- w) 2))(string-to-char " "))
                       "." (make-string (/ (1- w) 2)(string-to-char " ")) "\""))))
         (sa (concat s1 ",\n" s2 ",\n")))
    (eval `(concat "/* XPM */
static char * dot_vline_xpm[] = {
\"" (number-to-string w) " " (number-to-string h) " 2 1\",
\"  c None\",
\". c " color "\",\n"
,@(mapcar (lambda(x) sa)
          (make-list (1- (/ h 2)) 0))
s1 ",\n" s2 "};"
))))

(defvar ivl-line-height (or (car (window-line-height)) 20))
(defvar indent-vline-img (make-vline-xpm 9 ivl-line-height "#4D4D4D"))
(defvar indent-vline-img-lst (make-vline-xpm 9 ivl-line-height "#6a5acd"))
(defvar indent-vline-img-blk (make-vline-xpm 9 ivl-line-height "khaki"))

(defun erase-indent-vline (overlay after? beg end &optional length)
  (mapc
   (lambda(x)(delete-overlay x))
   (eval (overlay-get overlay 'auxline-id)))
  (font-lock-fontify-buffer)
  )

(defun draw-indent-tab (beg end id &optional img color)
  (let ((img (or img indent-vline-img))
        (color (or color "#4D4D4D"))
        (ov (make-overlay beg end)))
    (overlay-put ov 'auxline-id id)
    (overlay-put ov 'display
                 (if (display-images-p)
                     `(display (image
                                :type xpm
                                :data ,img
                                :pointer text
                                :ascent center
                                :mask (heuristic t))
                               rear-nonsticky (display)
                               fontified t)))
    (overlay-put ov 'modification-hooks '(erase-indent-vline))
    (overlay-put ov 'insert-in-front-hooks '(erase-indent-vline))
    (overlay-put ov 'insert-behind-hooks '(erase-indent-vline))
    ov))

;; (if (display-images-p)
;;         (set-text-properties
;;          beg end
;;          `(display (image
;;                     :type xpm
;;                     :data ,img
;;                     :pointer text
;;                     :ascent center
;;                     :mask (heuristic t))
;;                    rear-nonsticky (display)
;;                    fontified t))
;;       (compose-region
;;        beg end
;;        (prog1 "|"
;;          (set-text-properties beg end `(font-lock-face (:foreground ,color))))
;;        'decompose-region))

(defun draw-indent-vline (&optional column img color)
  (interactive "P")
  (save-excursion
    (let* ((line (intern (concat "auxline-" (number-to-string (random 10000)))))
           (i (or column (current-indentation))))
      (set line nil)
      (while (< i (if (<= (point-max)(line-end-position))
                      0
                    (forward-line)
                    (beginning-of-line)
                    (skip-chars-forward " ")
                    (current-column)))
        (move-to-column i)
        (let* ((p1 (point))(p2 (1+ p1)))
          (set line (cons (draw-indent-tab p1 p2 line img color) (eval line))))))))

(defun indent-vline (&optional regexp column img color)
  (interactive)
  (let ((x (or regexp "^")))
    (font-lock-add-keywords
     nil `((,x
            (0 (draw-indent-vline ,column ,img ,color)))))))

(defun draw-indent-vline-lisp-1 ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let* ((i (current-indentation))
           (l (save-excursion
                (count-lines (point)
                             (forward-list)))))
      (while (> l 0)
        (let* ((p1 (progn (move-to-column i)(point)))
               (p2 (1+ p1)))
          (if (and (eq (get-byte p1) 32)
                   (save-excursion
                     (skip-chars-backward " ")(bolp)))
              (draw-indent-tab p1 p2))
          nil)
        (forward-line)
        (setq l (1- l))))))

(defun indent-vline-0 (&optional regexp img color)
  (interactive)
  (let ((x (or regexp "   \\( \\)")))
    (font-lock-add-keywords
     nil `((,x
            (0 (if (save-excursion
                     (skip-chars-backward " ")(bolp))
                   (let* ((p1 (point))
                          (p2 (1+ p1)))
                     (if (or (null (eq (get-byte p1) 32))
                             (get-text-property p1 'display))
                         nil
                       (draw-indent-tab p1 p2 img color)
                       nil)))))))))

;; (defun indent-vline-lisp (&optional regexp)
;;   (interactive)
;;   (indent-vline (or regexp "^[ \t]*[,`#'(]")))

;(add-hook 'post-command-hook 'font-lock-fontify-buffer nil t)

(defun indent-vline-lisp ()
  (interactive)
  (let ((c '(save-excursion
              (goto-char (match-beginning 1))
              (current-column)))
        (blk "\\((let\\*?\\|(if\\|(while\\|(cond\\|(map.*\\|(defun\\|(save-excursion\\)"))
    (indent-vline "^[ \t]*\\((\\)" c)
    (indent-vline "\\((lambda\\|(setq\\|(defvar\\)" c 'indent-vline-img-lst)
    (indent-vline blk c 'indent-vline-img-blk)
    (indent-vline "[,`#']+\\((\\)" c 'indent-vline-img-lst))
  (font-lock-fontify-buffer))

(defun indent-vline-fixed(&optional img)
  (interactive)
  (indent-vline "^[ \t]*\\([^ \t]\\)"
                  '(save-excursion
                     (goto-char (match-beginning 1))
                     (current-column))
                  img)
  (font-lock-fontify-buffer))

(defun indent-vline-test (&optional regexp)
  (interactive)
  (indent-vline (or regexp "\\(def\\|class\\|if\\)")
                  '(save-excursion
                     (goto-char (match-beginning 1))
                     (current-column)))
  (font-lock-fontify-buffer))
