(defvar jdee-path (expand-file-name "jdee/lisp" exts-dir))
(add-to-list 'load-path jdee-path)
(load "jde")
(setq jde-check-version-flag nil)
(define-obsolete-function-alias 'make-local-hook 'ignore "21.1")
(unless (fboundp 'semantic-format-prototype-tag-java-mode)
  (defalias 'semantic-format-prototype-tag-java-mode 'semantic-format-tag-prototype-java-mode))
(require 'hippie-exp)
(custom-set-variables
 '(jde-jdk-registry (quote (("1.7" . "C:\\Program Files\\Java\\jdk1.7.0_25")))))

;;;###autoload
(autoload 'jde-mode (expand-file-name "jde" jdee-path) "" t)
