;;;###autoload
(defun xscheme ()
  "Loads xscheme and runs a scheme process in the current buffer."
  (interactive)
  (load-library "xscheme")
  (let* ((prefix (if (eq system-type 'windows-nt)
                     (expand-file-name "../../mit-scheme/" exec-directory)
                   "/usr"))
        (prg (expand-file-name "bin/mit-scheme.exe" prefix))
        (lib (expand-file-name "lib" prefix))
        (cmd (format "%s --library %s --emacs" prg lib))
        (buf (buffer-name (get-buffer-create "*MIT-Scheme*"))))
      (xscheme-start cmd buf buf)))
