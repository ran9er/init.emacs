;; -*- encoding: utf-8-unix; -*-
;; File-name:    <auto-hooks.el>
;; Create:       <2011-12-26 13:46:26 ran9er>
;; Time-stamp:   <2011-12-26 14:37:19 ran9er>
;; Mail:         <2999am@gmail.com>

(setq major-mode-list
      (let (y)
        (delete-dups
         (mapcar
          (lambda(x)
            (setq y (symbol-name (if (symbolp (cdr x))(cdr x)(car (last x)))))
            (substring (if (string-match "mode$" y) y (concat y "-mode")) 0 -5))
          auto-mode-alist))))

(let* ((dir (expand-file-name "_extensions/" *init-dir*))
       (ext (mapcar
             (lambda(x)(cons (file-name-sans-extension (file-name-nondirectory x)) x))
             (directory-files dir t "\\.el\\'"))))
  (if nil ;; if t ;; if t t
      ;; ** by major-mode
      (mapcar
       (lambda(x)
         (add-hook  (concat-symbol (car x) "-mode-hook")
                    `(lambda()(load ,(cdr x)))))
       ext)
    ;; ** by extension
    (add-hook 'find-file-hook
              `(lambda ()
                 (load (or
                        (cdr (assoc (or (file-name-extension (buffer-name))
                                        (and (string-equal "*" (substring (buffer-name) 0 1))
                                             (substring (buffer-name) 1 -1)))
                                    ',ext))
                        (make-temp-name ""))
                       t)))
    ))
