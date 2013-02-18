(setq skeleton-pair-cond-alist
      '(
        ((char-bf '(?$ ?=)) . (?\{ _ "}"))
        ((or (char-bf ?/)(char-bf ?=)) . (?\[ n _ n "]"))
        ((bolp) . (?/ "*" n  _  n "*/"))
        (t . (?/ _))
        ((bolp) . (?. -1 "->"))
        (t . (?. _))
        ))
(skeleton-pair-alist-update)

;;;###autoload
(autoload 'skeleton-pair-insert-maybe "skeleton" "" t)
