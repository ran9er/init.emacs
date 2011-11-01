;; -*- encoding: utf-8-unix; -*-
(require 'highlight-parentheses)

;; * colors 
(custom-set-variables '(hl-paren-colors (quote (
"MediumOrchid1"
"DeepSkyBlue1"
"cyan"
"medium sea green"
"green"
"lawn green"
"yellow"
))))

;; * Try using the following to integrate highlight-parentheses with autopair mode:

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (highlight-parentheses-mode)
             (setq autopair-handle-action-fns
                   (list 'autopair-default-handle-action
                         '(lambda (action pair pos-before)
                            (hl-paren-color-update))))))


;; * Better version to integrate highlight-parentheses with autopair mode:

;; (add-hook 'highlight-parentheses-mode-hook
;;           '(lambda ()
;;              (setq autopair-handle-action-fns
;;                    (append
;; 					(if autopair-handle-action-fns
;; 						autopair-handle-action-fns
;; 					  '(autopair-default-handle-action))
;; 					'((lambda (action pair pos-before)
;; 						(hl-paren-color-update)))))))

(add-hook 'emacs-lisp-mode-hook 'my-auto-pair)

;; * Enables highlight-parentheses-mode on all buffers:

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
