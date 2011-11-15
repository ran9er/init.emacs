;; -*- encoding: utf-8-unix; -*-
(deftheme turquoise "")
(custom-theme-set-faces
 'turquoise
 '(default ((t (:background "#1d4044" :foreground "#eeeeec"))))
 '(cursor ((t (:background "#fce94f" :foreground"#000000" ))))
 '(region ((t (:background "#107500"))))
 '(mode-line ((t (:foreground "#eeeeec" :background "#555753"))))
; '(mode-line-inactive ((t (:background "#e5e5e5" :foreground "#333333"))))
 '(fringe ((t (:foreground "#1e3f48" :background "#53798d"))))
 '(minibuffer-prompt ((t (:foreground "#89bbe1" :bold t))))
 '(font-lock-builtin-face ((t (:foreground "#ed6e80"))))
 '(font-lock-comment-face ((t (:foreground "#888a85"))))
 '(font-lock-constant-face ((t (:foreground "#008b8b"))))
 '(font-lock-function-name-face ((t (:foreground "#edd400"))))
 '(font-lock-keyword-face ((t (:foreground "#729fcf"))))
 '(font-lock-string-face ((t (:foreground "#ad7fa8"))))
 '(font-lock-type-face ((t (:foreground "#84c544"))))
 '(font-lock-variable-name-face ((t (:foreground "#38d8f0"))))
 '(font-lock-warning-face ((t (:foreground "Red" :bold t))))
 '(isearch ((t (:background "#cd00cd" :foreground "#b0e2ff"))))
 '(lazy-highlight ((t (:background "#afeeee"))))
 '(link ((t (:foreground "#0000ff" :underline t))))
 '(link-visited ((t (:foreground "#8b008b" :underline t))))
 '(button ((t (:underline t))))
 '(header-line ((t (:background "#e5e5e5" :foreground "#333333")))))
(provide-theme 'turquoise)

(deftheme eshell "")
(custom-theme-set-faces
 'eshell
 '(eshell-ls-archive ((t (:foreground "indianred1" :weight bold))))
 '(eshell-ls-backup ((t (:foreground "indianred3"))))
 '(eshell-ls-clutter ((t (:foreground "DimGray"))))
 '(eshell-ls-directory ((t (:weight bold :foreground "seagreen3"))))
 '(eshell-ls-executable ((t (:foreground "Coral"))))
 '(eshell-ls-missing ((t (:foreground "black"))))
 '(eshell-ls-picture ((t (:foreground "Violet")))) ; non-standard face
 '(eshell-ls-product ((t (:foreground "LightSalmon"))))
 '(eshell-ls-readonly ((t (:foreground "Aquamarine"))))
 '(eshell-ls-special ((t (:foreground "Gold"))))
 '(eshell-ls-symlink ((t (:foreground "White"))))
 '(eshell-ls-text ((t (:foreground "medium aquamarine")))) ; non-standard face
 '(eshell-ls-todo ((t (:weight bold :foreground "aquamarine")))) ; non-standard face
 '(eshell-ls-unreadable ((t (:foreground "DimGray"))))
 '(eshell-prompt ((t (:foreground "gold" :weight bold))))
)
(provide-theme 'eshell)

(deftheme forest "")
(custom-theme-set-faces
 'forest
 '(default ((t (:background "#0a2b1d" :foreground "#f8f8f8"))))
 '(cursor ((t (:background "#fce94f" :foreground "#edef7d"))))
 '(region ((t (:background "#107500"))))
 '(fringe ((t (:foreground "#233729" :background "#41a83e"))))
 '(mode-line ((t (:foreground "#96dd3b" :background "#336442"))))
 '(region ((t (:background "#91bb9e"))))
 '(minibuffer-prompt ((t (:foreground "#96dd3b" :bold t))))
 '(font-lock-builtin-face ((t (:foreground "#ffaa3e"))))
 '(font-lock-comment-face ((t (:foreground "#336442"))))
 '(font-lock-function-name-face ((t (:foreground "#f7e741"))))
 '(font-lock-keyword-face ((t (:foreground "#96dd3b"))))
 '(font-lock-string-face ((t (:foreground "#9df39f"))))
 '(font-lock-type-face ((t (:foreground"#91bb9e"))))
 '(font-lock-variable-name-face ((t (:foreground "#ffaa3e"))))
 '(font-lock-warning-face ((t (:foreground "#eb939a" :bold t))))
 '(header-line ((t (:background "#2f4f4f" :foreground "#778899"))))
)
(provide-theme 'forest)

(provide 'my-color-theme)
;; M-x list-colors-display

