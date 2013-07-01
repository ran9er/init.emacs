(defun recentf-interactive-complete ()
  "find a file in the recently open file using ido for completion"
  (interactive)
  (let* ((all-files recentf-list)
	 (file-assoc-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) all-files))
	 (filename-list (remove-duplicates (mapcar 'car file-assoc-list) :test 'string=))
	 (ido-make-buffer-list-hook
	  (lambda ()
	    (setq ido-temp-list filename-list)))
	 (filename (ido-read-buffer "Find Recent File: "))
	 (result-list (delq nil (mapcar (lambda (x) (if (string= (car x) filename) (cdr x))) file-assoc-list)))
	 (result-length (length result-list)))
         (find-file 
	  (cond 
	   ((= result-length 0) filename)
	   ((= result-length 1) (car result-list))
	   ( t
	     (let ( (ido-make-buffer-list-hook
		     (lambda ()
		       (setq ido-temp-list result-list))))
	       (ido-read-buffer (format "%d matches:" result-length))))
	   ))))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
