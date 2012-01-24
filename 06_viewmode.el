;; -*- encoding: utf-8-unix; -*-
;(define-key ctl-x-map "\C-q" 'view-mode)


(add-hook 'view-mode-hook (lambda nil
;; ** defun change-mode-line-color
(defun change-mode-line-color ()
  (interactive)
  (when (get-buffer-window (current-buffer))
    (cond (window-system
           (cond (view-mode
                  (set-face-foreground 'modeline "orange" #@8 "black")
                  )
                 (t
                  (set-face-foreground 'modeline "white" #@8 "black")))
           )
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
(progn
  (change-mode-line-color-advice set-window-configuration)
  (change-mode-line-color-advice switch-to-buffer)
  (change-mode-line-color-advice pop-to-buffer)
  (change-mode-line-color-advice other-window)
  (change-mode-line-color-advice toggle-read-only)
  (change-mode-line-color-advice vc-toggle-read-only)
  (change-mode-line-color-advice vc-next-action)
  (change-mode-line-color-advice view-mode-enable)
  (change-mode-line-color-advice view-mode-disable)
  (change-mode-line-color-advice bury-buffer)
  (change-mode-line-color-advice kill-buffer)
  (change-mode-line-color-advice delete-window)
  ;; for windows.el
  (change-mode-line-color-advice win-switch-to-window)
  (change-mode-line-color-advice win-toggle-window)
  )

(change-mode-line-color)
;; ** define-key
(define-key-s view-mode-map `(
             "p" View-scroll-page-backward
             "n" View-scroll-page-forward
             "k" View-scroll-line-backward
             "j" View-scroll-line-forward
             "f" View-search-last-regexp-forward
             "b" View-search-last-regexp-backward
             ))

(if outline-minor-mode (progn
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
