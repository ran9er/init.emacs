;;;###autoload
(defun bind-keys (map &rest kd)
  "bind some key like \"C-m\" \"C-i\" \"C-[\""
  (if window-system 
      (let* ((k (lambda (l)
                  (if l (cons
                         (list (nth 0 l) `[,(random)] (nth 1 l))
                         (funcall k (nthcdr 2 l))))))
             (l (funcall k kd))
             (m (or map (current-global-map))))
        (mapc
         (lambda(x)
           (define-key input-decode-map (eval `(kbd ,(nth 0 x)))(nth 1 x))
           (define-key m (nth 1 x) (nth 2 x)))
         l))))

;;;###autoload
(defun def-keys-low (map &rest kd)
  "bind some key like \"C-m\" \"C-i\" \"C-[\""
  (if window-system 
      (let* ((k (lambda (l)
                  (if l (cons
                         (list (nth 0 l) (random) (nth 1 l))
                         (funcall k (nthcdr 2 l))))))
             (l (funcall k kd))
             (m (or map (current-global-map))))
        (mapc
         (lambda(x)
           (keyboard-translate (nth 0 x) (nth 1 x))
           (define-key m `[,(nth 1 x)] (nth 2 x)))
         l))))
