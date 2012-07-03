;; -*- encoding: utf-8-unix; -*-
;; File-name:    <90_evil.el>
;; Create:       <2012-02-23 12:23:38 ran9er>
;; Time-stamp:   <2012-07-03 22:43:50 ran9er>
;; Mail:         <2999am@gmail.com>
(add-to-list 'load-path (expand-file-name "evil/" *init-dir*))
(autoload 'evil-mode "evil-core" nil t)
(defadvice evil-mode (before activate)
  (require 'evil)
  (setq evil-want-C-i-jump t
        evil-move-cursor-back nil))
;; (evil-mode 1)
