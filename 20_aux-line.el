;; -*- encoding: utf-8-unix; -*-
;; File-name:    <20_indent-vline.el>
;; Create:       <2012-01-18 00:53:10 ran9er>
;; Time-stamp:   <2012-01-23 19:32:44 ran9er>
;; Mail:         <2999am@gmail.com>

;; * hl-line
(require 'hl-line)
;; (global-hl-line-mode)
(set-face-attribute
 'hl-line nil
 :background
 (adjust-color
  (face-attribute 'default :background)
  -2))

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

(defvar indent-vline-img (make-vline-xpm 9 22 "#4D4D4D"))
(defvar indent-vline-img-lst (make-vline-xpm 9 22 "#6a5acd"))
(defvar indent-vline-img-blk (make-vline-xpm 9 22 "khaki"))
(defun draw-indent-tab (beg end &optional img color)
  (let ((img (or img indent-vline-img))
        (color (or color "#4D4D4D")))
    (if (display-images-p)
        (set-text-properties
         beg end
         `(display (image
                    :type xpm
                    :data ,img
                    :pointer text
                    :ascent center
                    :mask (heuristic t))
                   rear-nonsticky (display)
                   fontified t))
      (compose-region
       beg end
       (prog1 "|"
         (set-text-properties beg end `(font-lock-face (:foreground ,color))))
       'decompose-region))))

(defun draw-indent-vline (&optional column img color)
  (interactive "P")
  (save-excursion
    (let* ((i (or column (current-indentation))))
      (while (< i (if (null (zerop (forward-line)))
                      nil
                    (beginning-of-line)
                    (skip-chars-forward " ")
                    (current-column)))
        (move-to-column i)
        (let* ((p1 (point))(p2 (1+ p1)))
          (if (get-text-property p1 'display)
              nil
            (draw-indent-tab p1 p2 img color)))))))

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

(defun indent-vline-s (&optional regexp column img color)
  (interactive)
  (let ((x (or regexp "^")))
    (font-lock-add-keywords
     nil `((,x
            (0 (draw-indent-vline ,column ,img ,color))))))
  (defadvice delete-char (after indent-vline activate compile)
    (save-excursion
      (let* ((p (point))
             (q (skip-chars-forward " "))
             (x (progn (skip-chars-backward " ")(bolp))))
        (if x
            (remove-text-properties p (+ p q) '(display)))))))

(defun indent-vline (&optional regexp img color)
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
                       nil))))))))
  (defadvice delete-char (after indent-vline activate compile)
    (save-excursion
      (let* ((p (point))
             (q (skip-chars-forward " "))
             (x (progn (skip-chars-backward " ")(bolp))))
        (if x
            (remove-text-properties p (+ p q) '(display)))))))

;; (defun indent-vline-lisp (&optional regexp)
;;   (interactive)
;;   (indent-vline-s (or regexp "^[ \t]*[,`#'(]")))

(defun indent-vline-lisp ()
  (interactive)
  (let ((c '(save-excursion
              (goto-char (match-beginning 1))
              (current-column)))
        (blk "\\((let\\*?\\|(if\\|(while\\|(cond\\|(map.*\\|(defun\\|(save-excursion\\)"))
    (indent-vline-s "^[ \t]*\\((\\)" c)
    (indent-vline-s "\\((lambda\\|(setq\\|(defvar\\)" c 'indent-vline-img-lst)
    (indent-vline-s blk c 'indent-vline-img-blk)
    (indent-vline-s "[,`#']+\\((\\)" c 'indent-vline-img-lst)
    ))

(defun indent-vline-test (&optional regexp)
  (interactive)
  (indent-vline-s "\\(def\\|class\\|if\\)"
                  '(save-excursion
                     (goto-char (match-beginning 1))
                     (current-column))))

