(add-to-list 'load-path (expand-file-name "evil/" exts-dir))

(require 'evil)
(setq evil-want-C-i-jump t
      evil-move-cursor-back nil)
(define-key evil-normal-state-map " " 'scroll-up-command)
(define-key evil-motion-state-map " " 'scroll-up-command)

(defalias 'evil 'evil-mode)

;(evil-mode 1)

;; Disable evil for certain major-modes
(setq evil-disabled-modes-list
      '(eshell-mode
        wl-summary-mode
        compilation-mode
        completion-list-mode
        help-mode))

(when nil
  (defun evil-initialize ()
    (unless (or (minibufferp) (member major-mode evil-disabled-modes-list))
      (evil-local-mode 1)))
  )

;;;###autoload
(autoload 'evil-mode "evil-core" nil t)

