;; -*- encoding: utf-8-emacs-unix; -*-

;; * background 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if t t;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
     '(default ((t (:stipple nil :background ((image :type jpeg :file "/Path/to/your/image.png") :origin display) :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 101 :width normal :family "misc-fixed")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * toggle-comment-region
;; (defun toggle-comment-region (beg end &optional n)
;;   "Comment the lines in the region if the first non-blank line is
;; commented, and conversely, uncomment region. If optional prefix arg
;; N is non-nil, then for N positive, add N comment delimiters or for N
;; negative, remove N comment delimiters.
;; Uses `comment-region' which does not place comment delimiters on
;; blank lines."
;;   (interactive "r\nP")
;;   (if n
;;       (comment-region beg end (prefix-numeric-value n))
;;     (save-excursion
;;       (goto-char beg)
;;       (beginning-of-line)
;;       ;; skip blank lines
;;       (skip-chars-forward " \t\n")
;;       (if (looking-at (concat "[ \t]*\\(" (regexp-quote comment-start) "+\\)"))
;;           (uncomment-region beg end)
;;         (comment-region beg end)))))
;; * make-temp-file
;; (make-temp-file
;;       (format-time-string "%Y%m%d%H%M%S-" (current-time))
;;       nil
;;       suf)
