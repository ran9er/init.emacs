;; -*- encoding: utf-8-unix; -*-
;; File-name:    <ruby-mode.el>
;; Create:       <2012-01-08 00:44:15 ran9er>
;; Time-stamp:   <2012-01-08 00:55:32 ran9er>
;; Mail:         <2999am@gmail.com>
(load-once
 ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/ruby")  
 ;; (autoload 'ruby-mode "ruby-mode"  
 ;;   "Mode for editing ruby source files")  

 (if (eq system-type 'windows-nt)
     (mapc (lambda (p)(add-exec-path p))
           (list
            (expand-file-name "../../ruby/bin/" exec-directory)
            exec-directory)))

 (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))  
 (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))  
 (autoload 'run-ruby "inf-ruby"  
   "Run an inferior Ruby process")  
 (autoload 'inf-ruby-keys "inf-ruby"  
   "Set local key defs for inf-ruby in ruby-mode")  
 ;; (add-hook 'ruby-mode-hook  
 ;;           '(lambda ()  
 ;;              (inf-ruby-keys)))  
 ;; If you have Emacs 19.2x or older, use rubydb2x                               
 (autoload 'rubydb "rubydb3x" "Ruby debugger" t)  
 ;; uncomment the next line if you want syntax highlighting                      
 ;; (add-hook 'ruby-mode-hook 'turn-on-font-lock)  
 )

(inf-ruby-keys)
(turn-on-font-lock)
