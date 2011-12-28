;; -*- encoding: utf-8-unix; -*-
;; File-name:    <file.el>
;; Create:       <2011-12-27 21:32:02 ran9er>
;; Time-stamp:   <2011-12-28 10:12:29 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun find-temp (&optional suffix)
  (interactive "sExtension: ")
  (let ((suf (if (and suffix (null (string= suffix "")))
                 (concat "." suffix))))
    (find-file
     (concat
      (make-temp-name
       (expand-file-name
        (format-time-string "%Y%m%d%H%M%S-" (current-time))
        work-dir))
      suf))
    (run-hooks 'find-temp-hook)))

;;;###autoload
(defun write-temp (filename &optional confirm)
  (interactive
   (list (if buffer-file-name
             (read-file-name "Write file: "
                             nil nil nil nil)
           (read-file-name "Write file: " default-directory
                           (expand-file-name
                            (file-name-nondirectory (buffer-name))
                            default-directory)
                           nil nil))
         (not current-prefix-arg)))
  (let ((fnm buffer-file-name))
    (write-file filename confirm)
    (if (file-exists-p fnm)
        (delete-file fnm))))

(add-hook 'find-temp-hook (lambda ()
                            (yank)))

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
