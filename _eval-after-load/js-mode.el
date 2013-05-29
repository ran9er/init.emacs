;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;; you can;t have a symbol called "someName:false"

;; (require 'js2-mode)
;; (add-hook 'js2-post-parse-callbacks 
;;           (lambda ()
;;             (let ((btext (replace-regexp-in-string
;;                           ": *true" " "
;;                           (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
;;               (setq js2-additional-externs
;;                     (split-string
;;                      (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
;;                      " *, *" t))
;;               )))

(add-hook 'javascript-mode-hook 'skeleton-pair-alist-update)
