;; -*- encoding: utf-8-unix; -*-
;; File-name:    <20_indent-vline.el>
;; Create:       <2012-01-18 00:53:10 ran9er>
;; Time-stamp:   <2012-01-19 00:58:45 ran9er>
;; Mail:         <2999am@gmail.com>
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

(defvar indent-vline-img (make-vline-xpm 9 20 "#4D4D4D"))
(defun draw-indent-tab (beg end &optional color)
  (if window-system
      (set-text-properties
       beg end
       `(display (image
                  :type xpm
                  :data ,indent-vline-img
                  :pointer text
                  :ascent center
                  :mask (heuristic t))
                 rear-nonsticky (display)
                 fontified t))
    (compose-region
     beg end
     (prog1 "|"
       (set-text-properties beg end '(font-lock-face (:foreground "#4D4D4D"))))
     'decompose-region)))

(defun draw-indent-vline ()
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

(defun indent-vline-lisp ()
  (interactive)
  (funcall
   (lambda (x)
     (font-lock-add-keywords
      nil `((,x
             (0 (draw-indent-vline))))))
   "^[ \t]*[,`#'(]")
  (defadvice delete-char (after indent-vline activate compile)
    (save-excursion
      (let* ((p (point))
             (q (skip-chars-forward " "))
             (x (progn (skip-chars-backward " ")(bolp))))
        (if x
            (remove-text-properties p (+ p q) '(display)))))))

(defun indent-vline ()
  (interactive)
  (funcall
   (lambda (x)
     (font-lock-add-keywords
      nil `((,x
             (0 (if (save-excursion
                      (skip-chars-backward " ")(bolp))
                    (let* ((p1 (point))
                           (p2 (1+ p1)))
                      (if (or (null (eq (get-byte p1) 32))
                              (get-text-property p1 'display))
                          nil
                        (draw-indent-tab p1 p2)
                        nil))))))))
   "   \\( \\)")
  (defadvice delete-char (after indent-vline activate compile)
    (save-excursion
      (let* ((p (point))
             (q (skip-chars-forward " "))
             (x (progn (skip-chars-backward " ")(bolp))))
        (if x
            (remove-text-properties p (+ p q) '(display)))))))
