(add-exec-path (expand-file-name "../../Racket"))
(add-exec-path (expand-file-name "../../mit-scheme/bin"))

(setq scheme-program-name (if (eq system-type 'windows-nt)
                                "Racket" "guile"))

(setq quack-default-program scheme-program-name)

;; (setq quack-fontify-style nil)

;;;###autoload
(autoload 'run-scheme (expand-file-name "quack" exts-dir)
  "Quack scheme editing mode" t)

;;;###autoload
(autoload 'scheme-mode (expand-file-name "quack" exts-dir)
  "Quack scheme editing mode" t)
