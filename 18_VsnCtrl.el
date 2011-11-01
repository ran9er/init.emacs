;; -*- encoding: utf-8-unix; -*-
;; * magit
(add-to-list 'load-path (expand-file-name "magit/" init-dir))
(autoload 'magit-status "magit" nil t)

(add-hook 'magit-mode-hook '(lambda () (def-k-s magit-mode-map
    "M-4" delete-window
    "M-1" delete-other-windows
    "M-2" split-window-vertically
    "M-3" split-window-horizontally
    "M-0" other-window "M-o" other-window
    "1" magit-show-level-1-all
    "2" magit-show-level-2-all
    "3" magit-show-level-3-all
    "4" magit-show-level-4-all
    "C-1" magit-show-level-1
    "C-2" magit-show-level-2
    "C-3" magit-show-level-3
    "C-4" magit-show-level-4
    )))

(custom-set-faces
 '(magit-diff-add ((t (:foreground "lime green"))))
 '(magit-diff-del ((t (:foreground "tomato"))))
 '(magit-item-highlight ((t (:background "#123a26"))))
 '(magit-log-head-label-patches ((t (:background "DarkGoldenrod1" :foreground "DarkOrange4" :box 1))))
 '(magit-log-tag-label ((t (:background "blue violet"))))
)
;; * egit
;; (add-to-list 'load-path (expand-file-name "egit/" init-dir))
;; (autoload 'egit "egit" "Emacs git history" t)
;; (autoload 'egit-file "egit" "Emacs git history file" t)
;; (autoload 'egit-dir "egit" "Emacs git history directory" t)

;; * egg
;(add-to-list 'load-path (expand-file-name "egg/" init-dir))
;(require 'egg)

;; * git-emacs
;; (add-to-list 'load-path (expand-file-name "git-emacs/" init-dir))
;; (fmakunbound 'git-status)
;; (require 'git-emacs-autoloads)

;; * diff mode hook
(add-hook 'diff-mode-hook
      (function (lambda ()
              (setq outline-regexp "diff\\|@@\\|*\\{10,\\}")
              (setq outline-heading-alist
                '(("diff" . 1) ("@@" . 2) ("*\\{10,\\}" . 2))
                )
              (outline-minor-mode)
;             (hide-body)
              (view-mode)
              )))
