;; -*- encoding: utf-8-unix; -*-
;(define-key ctl-x-map "\C-q" 'view-mode)

(eval-after-load 'view
  '(progn
     (define-key-s view-mode-map
       `("p" View-scroll-page-backward
         "n" View-scroll-page-forward
         "k" View-scroll-line-backward
         "j" View-scroll-line-forward
         "f" View-search-last-regexp-forward
         "b" View-search-last-regexp-backward
         ))
     ;; ** defun change-mode-line-color
     (defun change-mode-line-color ()
       (interactive)
       (when (get-buffer-window (current-buffer))
         (cond (window-system
                (cond (view-mode
                       (set-face-foreground 'modeline "orange" #@8 "black"))
                      (t
                       (set-face-foreground 'modeline "white" #@8 "black"))))
               (t
                (set-face-background 'modeline
                                     (if view-mode "red"
                                       "white"))))))
     ;; ** defmacro change-mode-line-color-advice
     (defmacro change-mode-line-color-advice (f)
       `(defadvice ,f (after change-mode-line-color activate)
          (change-mode-line-color)
          (force-mode-line-update)))
     ;; ** progn
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
        win-toggle-window))
     ;; (change-mode-line-color)
     ))

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
