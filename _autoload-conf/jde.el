(defvar jdee-path (expand-file-name "jdee/lisp" exts-dir))
(add-to-list 'load-path jdee-path)
(load "jde")
(custom-set-variables
 '(jde-jdk-registry (quote (("1.7" . "C:\\Program Files\\Java\\jdk1.7.0_25")))))

;;;###autoload
(autoload 'jde-mode (expand-file-name "jde" jdee-path) "" t)
