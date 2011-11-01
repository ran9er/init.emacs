(if window-system
    (progn
      (def-k-s 0
        "C-7"     eshell                      ; C-m
        "C-8"     undo-tree-visualize         ; C-i
        "C-9"     magit-status                ; C-[
        )
      (def-k-s input-decode-map 
        "C-m" [?\C-7]   "C-7" [?\C-m]
        "C-i" [?\C-8]   "C-8" [?\C-i]
        "C-[" [?\C-9]   "C-9" [?\C-\[]
        )
      ))
