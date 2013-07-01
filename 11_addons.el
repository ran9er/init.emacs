;; -*- encoding: utf-8-unix; -*-
;; *========== hl-line
(require 'hl-line)
;; (global-hl-line-mode)
(set-face-attribute
 'hl-line nil
 :background
 (adjust-color
  (face-attribute 'default :background)
  -2))

;; *========== ibuffer
(require 'ibuffer)
;; (global-set-key ( kbd "C-x C-b ")' ibuffer)

;; *========== undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(defadvice undo-tree-visualizer-mode (after undo-tree-face activate)
  (buffer-face-mode))

;; *========== auto-complete
(global-auto-complete-mode)

;; *========== anything
;; (require 'anything)

;; *========== browse-kill-ring
(require 'browse-kill-ring)
(global-set-key [(control c)(k)] 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; *========== minibuffer-depth
(when enable-recursive-minibuffers
  (require 'mb-depth)
  (minibuffer-depth-indicate-mode))

;; *========== minibuffer-complete-cycle
(require 'minibuffer-complete-cycle)
(setq minibuffer-complete-cycle t)

;; *========== iedit
(autoload 'iedit-mode "iedit" "" t nil)

;; *========== linY
;; (liny-mode)

;; *========== multi-term
;; (require 'multi-term)
;; (if (eq system-type 'gnu/linux)
;;     (setq multi-term-program "/bin/bash"))

;; *========== mpg123
;; (autoload 'mpg123 "mpg123" "A Front-end to mpg123/ogg123" t)

;; *========== docbook
;; (require 'docbook-xml-mode)
;; (add-hook 'docbook-xml-mode-hook
;;           (function
;;            (lambda ()
;;              (setq outline-regexp "<!\\-\\-\\*+")
;;              (outline-minor-mode)
;;              (hide-body))))
