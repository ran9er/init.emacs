(add-to-list 'auto-mode-alist '("\\.wlua\\'" . lua-mode))

(add-hook 'lua-mode-hook 'skeleton-pair-alist-update)
