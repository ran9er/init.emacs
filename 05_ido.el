;; -*- encoding: utf-8-unix; -*-
(require 'ido)

(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

(add-hook 'ido-setup-hook 
          (lambda () 
            (define-key ido-completion-map "\C-w" 'ido-delete-backward-updir)
            (define-key ido-completion-map "\C-y" 'ido-copy-current-file-name)
))

(ido-mode t)


(setq ido-save-directory-list-file "~/.emacs.d/_ido_last")

;; M-x mode
 (defun ido-execute-extended-command ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "Run: "
      (all-completions "" obarray 'commandp)))))
