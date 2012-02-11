;; -*- encoding: utf-8-unix; -*-
;; *========== ibuffer
(require 'ibuffer)
;; (global-set-key ( kbd "C-x C-b ")' ibuffer)

;; *========== undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(defadvice undo-tree-visualizer-mode (after undo-tree-face activate)
  (buffer-face-mode))

;; *========== anything
;; (require 'anything)

;; *========== browse-kill-ring
(require 'browse-kill-ring)
(global-set-key [(control c)(k)] 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; *========== minibuffer-complete-cycle
(require 'minibuffer-complete-cycle)
(setq minibuffer-complete-cycle t)

;; *========== iedit
(autoload 'iedit-mode "iedit" "" t nil)

;; *========== multi-term
;; (require 'multi-term)
;; (if (eq system-type 'gnu/linux)
;;     (setq multi-term-program "/bin/bash"))

;; *========== docbook
;; (require 'docbook-xml-mode)
;; (add-hook 'docbook-xml-mode-hook
;;           (function
;;            (lambda ()
;;              (setq outline-regexp "<!\\-\\-\\*+")
;;              (outline-minor-mode)
;;              (hide-body))))
