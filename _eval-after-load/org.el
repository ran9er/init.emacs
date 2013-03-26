;; *========== org-mode	
(setq org-hide-leading-stars t)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done 'time)
(add-hook 'org-mode-hook
          (lambda () (setq truncate-lines nil)))
