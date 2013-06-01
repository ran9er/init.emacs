(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

;; *========== lua
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.wlua\\'" . lua-mode))
(autoload 'lua-mode "lua-mode" "" t nil)

;; (setq auto-mode-alist
;;       (remove '("\\.js\\'" . javascript-generic-mode) auto-mode-alist))

;; *========== markdown
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)

