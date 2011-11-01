;; -*- encoding: utf-8-unix; -*-
;; *========== ibuffer		
(require 'ibuffer)
;(global-set-key ( kbd "C-x C-b ")' ibuffer)

;; *========== undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; *========== run-current-file-macro
(autoload 'run-current-file-macro "run-file" "" t)

(global-set-key (kbd "<f5>") (run-current-file-macro compile))
(global-set-key (kbd "<f6>") (run-current-file-macro shell-command))

;; *========== org-mode	
(setq org-hide-leading-stars t)
 (define-key global-map "\C-ca" 'org-agenda)
 (setq org-log-done 'time)


;; *========== browse-kill-ring
(require 'browse-kill-ring) 
(global-set-key [(control c)(k)] 'browse-kill-ring) 
(browse-kill-ring-default-keybindings) 

;; *========== minibuffer-complete-cycle
(require 'minibuffer-complete-cycle)
(setq minibuffer-complete-cycle t)

;; *========== multi-term
;	(require 'multi-term)
;	
;	(if (eq system-type 'gnu/linux)
;	  (setq multi-term-program "/bin/bash")
;	)


;; *========== docbook
;(require 'docbook-xml-mode)

;(add-hook 'docbook-xml-mode-hook
;	  (function (lambda ()
;                  (setq outline-regexp "<!\\-\\-\\*+")
;		      (outline-minor-mode)
;		      (hide-body)
;	    		)))
