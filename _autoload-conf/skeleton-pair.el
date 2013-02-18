(setq skeleton-pair-cond-alist
      '(
        ((char-bf '(?$ ?=)) . (?\{ _ "}"))
        ((char-bf '(?+ ?-)) . (?\\ "+"  _  "+\\"))
        ((or (char-bf ?/)(char-bf ?=)) . (?\[ n _ n "]"))
        ((bolp) . (?/ "*" n  _  n "*/"))
        (t . (?/ _))
        ))
(skeleton-pair-alist-update)

;;;###autoload
(autoload 'skeleton-pair-insert-maybe "skeleton" "" t)
