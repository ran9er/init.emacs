;; -*- encoding: utf-8-unix; -*-
;; File-name:    <my-color-themes.el>
;; Create:       <2011-11-20 10:44:36 ran9er>
;; Time-stamp:   <2012-01-15 11:58:32 ran9er>
;; Mail:         <2999am@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'color-theme)

(defun color-theme-turquoise ()
  (interactive)
  (color-theme-install
   '(color-theme-turquoise
     ((background-color . "#1d4044")
      (background-mode . light)
      (border-color . "#53798d")
      (cursor-color . "#fce94f")
      (foreground-color . "#eeeeec")
      (mouse-color . "black"))
     (fringe ((t (:foreground "#1e3f48" :background "#53798d"))))
     (mode-line ((t (:foreground "#eeeeec" :background "#555753"))))
     (region ((t (:background "#107500"))))
     (font-lock-builtin-face ((t (:foreground "#ed6e80"))))
     (font-lock-comment-face ((t (:foreground "#888a85"))))
     (font-lock-function-name-face ((t (:foreground "#edd400"))))
     (font-lock-keyword-face ((t (:foreground "#729fcf"))))
     (font-lock-string-face ((t (:foreground "#ad7fa8"))))
     (font-lock-type-face ((t (:foreground "#84c544"))))
     (font-lock-variable-name-face ((t (:foreground "#38d8f0"))))
     (minibuffer-prompt ((t (:foreground "#89bbe1" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
)))

(defun color-theme-eshell ()
  (interactive)
  (color-theme-install
   '(color-theme-eshell
     nil
     (eshell-ls-archive-face ((t (:bold t :foreground "indianred1"))))
     (eshell-ls-backup-face ((t (:foreground "indianred3"))))
     (eshell-ls-clutter-face ((t (:foreground "DimGray"))))
     (eshell-ls-directory-face ((t (:bold t :foreground "seagreen3"))))
     (eshell-ls-executable-face ((t (:foreground "Coral"))))
     (eshell-ls-missing-face ((t (:foreground "black"))))
     (eshell-ls-picture-face ((t (:foreground "Violet")))) ; non-standard face
     (eshell-ls-product-face ((t (:foreground "LightSalmon"))))
     (eshell-ls-readonly-face ((t (:foreground "Aquamarine"))))
     (eshell-ls-special-face ((t (:foreground "Gold"))))
     (eshell-ls-symlink-face ((t (:foreground "White"))))
     (eshell-ls-text-face ((t (:foreground "medium aquamarine")))) ; non-standard face
     (eshell-ls-todo-face ((t (:bold t :foreground "aquamarine")))) ; non-standard face
     (eshell-ls-unreadable-face ((t (:foreground "DimGray"))))
     (eshell-prompt-face ((t (:bold t :foreground "gold"))))
)))

(defun color-theme-forest ()
  (interactive)
  (color-theme-install
   '(color-theme-forest
      (
       (background-color . "#0a2b1d")
      (background-mode . light)
      (border-color . "#edef7d")
      (cursor-color . "#edef7d")
      (foreground-color . "#f8f8f8")
      (mouse-color . "black")
)
     (fringe ((t (:foreground "#233729" :background "#41a83e"))))
     (mode-line ((t (:foreground "#96dd3b" :background "#336442"))))
     (region ((t (:background "#91bb9e"))))
     (font-lock-builtin-face ((t (:foreground "#ffaa3e"))))
     (font-lock-comment-face ((t (:foreground "#336442"))))
     (font-lock-function-name-face ((t (:foreground "#f7e741"))))
     (font-lock-keyword-face ((t (:foreground "#96dd3b"))))
     (font-lock-string-face ((t (:foreground "#9df39f"))))
     (font-lock-type-face ((t (:foreground"#91bb9e"))))
     (font-lock-variable-name-face ((t (:foreground "#ffaa3e"))))
     (minibuffer-prompt ((t (:foreground "#96dd3b" :bold t))))
     (font-lock-warning-face ((t (:foreground "eb939a" :bold t))))
     )))

(autoload 'color-theme-robin-hood "color-theme-robin-hood" t "")
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
;    (color-theme-robin-hood)
     (if window-system (color-theme-forest))
;    (color-theme-robin-hood)
;    (set-face-attribute 'fringe nil :foreground  (background-color-at-point))
     )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


