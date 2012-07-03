;; -*- encoding: utf-8-unix; -*-
;; File-name:    <90_evil.el>
;; Create:       <2012-02-23 12:23:38 ran9er>
;; Time-stamp:   <2012-07-04 00:01:21 ran9er>
;; Mail:         <2999am@gmail.com>
(add-to-list 'load-path (expand-file-name "evil/" *init-dir*))
(autoload 'evil-mode "evil-core" nil t)
(defadvice evil-mode (before activate)
  (require 'evil)
  (setq evil-want-C-i-jump t
        evil-move-cursor-back nil))
(eval-after-load 'evil
  '(progn
     (define-key evil-normal-state-map " " 'scroll-up-command)
     (define-key evil-motion-state-map " " 'scroll-up-command)))
(defalias 'evil 'evil-mode)


;; (defadvice evil-local-mode (around m activate)
;;   (if (null (member major-mode '(eshell-mode emacs-lisp-mode)))
;;       ad-do-it))

;; (evil-mode 1)
