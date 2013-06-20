(define-key-s view-mode-map
  `("p" View-scroll-page-backward
    "n" View-scroll-page-forward
    "k" View-scroll-line-backward
    "j" View-scroll-line-forward
    "f" View-search-last-regexp-forward
    "b" View-search-last-regexp-backward
    ))

;; ** defun change-mode-line-color
(defvar viewmode-mode-line-face
  (mapcar
   (lambda(x)
     (mapcar
      (lambda(y)
        (adjust-color (face-attribute 'mode-line y) x))
      '(:foreground :background)))
   '(-40 0)))

(defun change-mode-line-color ()
  (interactive)
  (when (get-buffer-window (current-buffer))
    (cond (window-system
           (set-face-foreground 'mode-line
                                (if buffer-read-only (nth 0 (nth 0 viewmode-mode-line-face))
                                  (nth 0 (nth 1 viewmode-mode-line-face)))))
          (t
           (set-face-background 'mode-line
                                (if buffer-read-only (nth 1 (nth 0 viewmode-mode-line-face))
                                  (nth 1 (nth 1 viewmode-mode-line-face))))))))
;; ** defmacro change-mode-line-color-advice
(defmacro change-mode-line-color-advice (f)
  `(defadvice ,f (after change-mode-line-color activate)
     (change-mode-line-color)
     (force-mode-line-update)))
;; ** progn
(when t
  (mapc
   (lambda(x)
     (eval
      `(change-mode-line-color-advice ,x)))
   '(set-window-configuration
     switch-to-buffer
     pop-to-buffer
     other-window
     toggle-read-only
     vc-toggle-read-only
     vc-next-action
     view-mode-enable
     view-mode-disable
     bury-buffer
     kill-buffer
     delete-window
     ;; for windows.el
     win-switch-to-window
     win-toggle-window)))
;; (change-mode-line-color)
(add-hook
 'view-mode-hook
 (lambda nil
   ;; ** define-key
   (if outline-minor-mode
       (progn
         (make-local-variable 'vol-map)
         (setq vol-map (make-sparse-keymap))
         (define-key view-mode-map "o" vol-map)
         (def-k-s vol-map
           "k" outline-previous-visible-heading
           "j" outline-next-visible-heading
           "p" outline-backward-same-level
           "n" outline-forward-same-level
           "u" outline-up-heading
           "i" hide-body
           "o" show-subtree
           "SPC" outline-cycle
           )))

   ;; *
   ))
