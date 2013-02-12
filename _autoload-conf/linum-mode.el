;;;###autoload
(autoload 'linum-mode "linum" "" t)

;(global-linum-mode)

;; Disable linum for certain major-modes
(setq linum-disabled-modes-list
      '(eshell-mode
        magit-status-mode
        wl-summary-mode
        compilation-mode
        completion-list-mode
        help-mode))

(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
    (linum-mode 1)))

;; relative line num
(defvar my-linum-format-string "%3d")

(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

(defun my-linum-get-format-string ()
  (let* ((width (1+ (length (number-to-string
                             (count-lines (point-min) (point-max))))))
         (format (concat "%" (number-to-string width) "d")))
    (setq my-linum-format-string format)))

(defvar my-linum-current-line-number 0)

(setq linum-format 'my-linum-relative-line-numbers)

(defun my-linum-relative-line-numbers (line-number)
  (let ((offset (- line-number my-linum-current-line-number)))
    (propertize (format my-linum-format-string offset) 'face 'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)
;(ad-deactivate 'linum-update)
