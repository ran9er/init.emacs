;; -*- encoding: utf-8-unix; -*-
;; File-name:    <09_modes.el>
;; Create:       <2012-07-28 23:43:36 ran9er>
;; Time-stamp:   <2012-07-28 23:44:55 ran9er>
;; Mail:         <2999am@gmail.com>

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(autoload 'lua-mode "lua-mode" "" t nil)

;; (setq auto-mode-alist
;;       (remove '("\\.js\\'" . javascript-generic-mode) auto-mode-alist))

;; *========== markdown
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)

