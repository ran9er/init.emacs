;; -*- encoding: utf-8-unix; -*-
;; File-name:    <20_indent-vline.el>
;; Create:       <2012-01-18 00:53:10 ran9er>
;; Time-stamp:   <2012-07-10 20:39:59 ran9er>
;; Mail:         <299am@gmail.com>

(setq indent-line-prefix "auxline-"
      indent-line-key 'indent-line-id
      indent-line-bg 'indent-line-bg)

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

(defun kill-indent-vline (m &optional n)
  (let ((n (or n (1+ m))))
    (mapc
     (lambda(x)(let ((i (overlay-get x indent-line-key)))
             (if i
                 (progn
                   (mapc
                    (lambda(y)(delete-overlay y))
                    (eval i))
                   (unintern i)))))
     (overlays-in m n))))
(defun erase-indent-vline (overlay after? beg end &optional length)
  (if after?
      (let* (i)
        (save-excursion
          (forward-line)
          (setq i (point))
          (kill-indent-vline
           i (+ i (skip-chars-forward " "))))
        (font-lock-fontify-block))
    (let* ((p1 (point))
           (b (save-excursion (skip-chars-forward " ")))
           (p2 (+ p1 b)))
      (kill-indent-vline p1 p2))))

(defun draw-indent-tab (beg end id &optional img color)
  (let ((img (or img indent-vline-img))
        (color (or color "#4D4D4D"))
        (ov (make-overlay beg end)))
    (overlay-put ov indent-line-key id)
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
    ov))

;; (add-hook 'pre-command-hook 'erase-indent-vline)
;; (add-hook 'post-command-hook 'font-lock-fontify-block)

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

(defun indent-line-overlay-exist (p k)
  (let (r (l (overlays-at p)))
    (while (and l
                (null
                 (if (overlay-get (car l) k)
                     (setq r t)
                   nil)))
      (setq l (cdr l)))
    r))
(defun draw-indent-vline (&optional column img color)
  (interactive "P")
  (save-excursion
    (let* ((line (intern (concat "*" indent-line-prefix (number-to-string (random 10000)) "*")))
           (i (or column (current-indentation))))
      (make-local-variable line)
      (set line nil)
      (while (< i (if (<= (point-max)(line-end-position))
                      0
                    (forward-line)
                    (beginning-of-line)
                    (skip-chars-forward " ")
                    (current-column)))
        (move-to-column i)
        (let* ((p1 (point))(p2 (1+ p1)))
          (if (indent-line-overlay-exist p1 indent-line-key)
              nil
            (set line (cons (draw-indent-tab p1 p2 line img color) (eval line)))))))))

(defun indent-vline-background ()
  (let* ((p (line-beginning-position))
         (i (current-indentation))
         (q (+ p i))
         o)
    (if (indent-line-overlay-exist p indent-line-bg)
        nil
      (setq o (make-overlay p q))
      (overlay-put o indent-line-bg t)
      (overlay-put o 'modification-hooks '(erase-indent-vline))
      (overlay-put o 'insert-in-front-hooks '(erase-indent-vline))
      (overlay-put o 'insert-behind-hooks '(erase-indent-vline)))))

(defun indent-vline (&optional regexp column img color)
  (interactive)
  (let ((x (or regexp "^")))
    (font-lock-add-keywords
     nil `((,x
            (0 (draw-indent-vline ,column ,img ,color)))))
    (font-lock-add-keywords
     nil '(("^ +"
            (0 (indent-vline-background)))))))


;; (defun indent-vline-lisp (&optional regexp)
;;   (interactive)
;;   (indent-vline (or regexp "^[ \t]*[,`#'(]")))

(defun indent-vline-current-column ()
  (save-excursion
    (goto-char (match-beginning 1))
    (current-column)))

(defun indent-vline-lisp ()
  (interactive)
  (let ((c '(indent-vline-current-column))
        (blk "\\((let\\*?\\|(if\\|(while\\|(cond\\|(map.*\\|(defun\\|(save-excursion\\)"))
    (indent-vline "^[ \t]*\\((\\)" c)
    (indent-vline "\\((lambda\\|(setq\\|(defvar\\)" c 'indent-vline-img-lst)
    (indent-vline blk c 'indent-vline-img-blk)
    (indent-vline "[,`#']+\\((\\)" c 'indent-vline-img-lst))
  (font-lock-fontify-buffer))

(defun indent-vline-fixed(&optional img)
  (interactive)
  (indent-vline "^[ \t]*\\([^ \t]\\)"
                  '(indent-vline-current-column)
                  img)
  (font-lock-fontify-buffer))

(defun indent-vline-test (&optional regexp)
  (interactive)
  (indent-vline (or regexp "\\(def\\|class\\|if\\)")
                  '(save-excursion
                     (goto-char (match-beginning 1))
                     (current-column)))
  (font-lock-fontify-buffer))
