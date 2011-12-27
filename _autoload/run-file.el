;; -*- encoding: utf-8-unix; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; F5 运行当前文件 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defmacro run-current-file-macro (outputf)
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl,
then it'll call “perl x.pl” in a shell.
The file can be php, perl, python, bash, java.
File suffix is used to determine what program to run."
  `(lambda()
     (interactive)
     (let (ext-map file-name file-ext prog-name cmd-str)
					; get the file name
					; get the program name
					; run it
       (setq ext-map
	     '(
	       ("php" . "php")
	       ("pl" . "perl")
	       ("py" . "python")
	       ("sh" . "bash")
	       ("java" . "javac")
           ("hs" .  "hugs")
	       )
	     )
       (setq file-name (buffer-file-name))
       (setq file-ext (file-name-extension file-name))
       (setq prog-name (cdr (assoc file-ext ext-map)))
       (setq cmd-str (concat prog-name " " file-name))
       (,outputf cmd-str))))

;; (global-set-key (kbd "<f5>") (run-current-file-macro compile))
;; (global-set-key (kbd "<f6>") (run-current-file-macro shell-command))
