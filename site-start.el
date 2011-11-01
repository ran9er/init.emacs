;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (boundp 'init-name);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq   init-name   "init.emacs"
        init-dir    (expand-file-name
                     (if (eq system-type 'windows-nt)
                         (concat exec-directory "../" init-name "/")
                       (concat "~/" init-name "/"))))

(if (file-exists-p init-dir)
 (progn
   (setq   init-start  (time-to-seconds))

   (dolist (init-load-path (cons init-dir (directory-files init-dir t "^_")))
     (if (file-directory-p init-load-path)
         (add-to-list 'load-path init-load-path)))

   (setq   init-files  (mapc 'load (directory-files init-dir t "\\.el\\'"))
           init-stop   (time-to-seconds))

   (add-hook 'emacs-startup-hook
             '(lambda ()
                (message "load %d init file , spend %g seconds ; startup spend %g seconds"
                         (length init-files)
                         (- init-stop init-start)
                         (- (time-to-seconds) init-start))))
))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
