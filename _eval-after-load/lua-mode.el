(add-to-list 'auto-mode-alist '("\\.wlua\\'" . lua-mode))

(add-hook 'lua-mode-hook
          (lambda()
            (set (make-local-variable 'skeleton-pair-cond-alist)
                 '(
                   (?\( . ((t _ ")")))
                   (?\{ . (((match-str-bf "(.*)\\|function") n _ n "}")
                           (t _ "}")))
                   (?\[ . (((match-str-bf "--") n _ n "--]")
                           (t _ "]")))
                   (?\" . ((t _ "\"")))
                   ))
            (skeleton-pair-alist-update)))
