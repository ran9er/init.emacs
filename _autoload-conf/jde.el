(defvar jdee-path (expand-file-name "jdee/lisp" exts-dir))
(add-to-list 'load-path jdee-path)
(load (expand-file-name "jde" jdee-path))
(setq jde-check-version-flag nil)
(define-obsolete-function-alias 'make-local-hook 'ignore "21.1")
(unless (fboundp 'semantic-format-prototype-tag-java-mode)
  (defalias 'semantic-format-prototype-tag-java-mode 'semantic-format-tag-prototype-java-mode))
(require 'hippie-exp)

;; JAVA_HOME
(unless (getenv "JAVA_HOME")
  (let ((java (if (eq system-type 'windows-nt)
                  "C:\\Program Files\\Java")))
    (setenv "JAVA_HOME" (car (nreverse (directory-files java t "jdk" 'string-lessp))))))
;; JRE_HOME
(setenv "JRE_HOME" (expand-file-name "jre" (getenv "JAVA_HOME")))
;; CLASSPATH
(when nil
  (mapc
   (lambda(x)
     (add-environment "CLASSPATH" (expand-file-name "lib" (getenv x))))
   '("JAVA_HOME" "JRE_HOME")))
(add-environment "CLASSPATH" ".")
;; JAVA_TOOL_OPTIONS
(setenv "JAVA_TOOL_OPTIONS" "-Dfile.encoding=utf-8 -Duser.language=cn -Duser.country=ZH")
;; JDK
(let* ((ver "1.7")
       (jdk (getenv "JAVA_HOME")))
  (custom-set-variables `(jde-jdk-registry (quote ((,ver . ,jdk))))))



;;;###autoload
(autoload 'jde-mode (expand-file-name "jde" jdee-path) "" t)
