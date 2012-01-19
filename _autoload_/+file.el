;; -*- encoding: utf-8-unix; -*-
;; File-name:    <file.el>
;; Create:       <2011-12-27 21:32:02 ran9er>
;; Time-stamp:   <2012-01-19 10:48:40 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun read-lines (filePath)
  "Return a list of lines in FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string
     (buffer-string) "\n" t)))

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
(defun run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl,
then it'll call “perl x.pl” in a shell.
The file can be php, perl, python, bash, java.
File suffix is used to determine what program to run."
  (interactive)
  (let (ext-map file-name file-ext prog-name cmd-str
                outputf status)
    (setq ext-map
          (to-alist
           '(
             "py"       "python"
             "rb"       "ruby"
             "lua"      "lua"
             "php"      "php"
             "pl"       "perl"
             "sh"       "bash"
             "java"     "javac"
             "hs"       "hugs"
             )))
    (setq
     status
     (catch 'status
       (or (setq file-name (buffer-file-name))
           (throw 'status "isn't a file!"))
       (or (setq file-ext (file-name-extension file-name))
           (throw 'status "no file ext name!"))
       (or (setq prog-name (cdr (assoc file-ext ext-map)))
           (throw 'status "how to do?"))
       (setq cmd-str (concat prog-name " " file-name))
       (setq outputf (if (equal (this-command-keys) [f5]) 'compile 'shell-command))
       (and (funcall outputf cmd-str)
            (throw 'status nil))))
    (and status (message "%s" status))))
