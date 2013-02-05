;; -*- encoding: utf-8-unix; -*-
;; * 配置书签
(setq bookmark-default-file "~/.emacs.d/emacs-bmk"
      bookmark-save-flag 1
      )

;; * Desktop Reload
(require 'desktop)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
;; use only one desktop
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name
      (concat "emacs-" (nth 2 (split-string (version))) "-desktop"))
(desktop-save-mode t)
(desktop-release-lock)

(setq desktop-clear-preserve-buffers
      (cons "\\*eshell\\*.*" desktop-clear-preserve-buffers))

;; desktop-frame
;(require 'desktop-frame)
;(add-hook 'desktop-save-hook
;           (lambda ()
;             (desktop-frame-save desktop-dirname)))
;(desktop-read)
;(load (expand-file-name "emacs.frx" desktop-dirname))

(defalias 'clear 'desktop-clear)
;(autoload 'my-clean-buffer "my-clean-buffer" t)
(defalias 'clean 'my-clean-buffer)

;; * recentf
(require 'recentf)
(recentf-mode 1)

(defun recentf-open-files-compl ()
  (interactive)
  (let* ((all-files recentf-list)
     (tocpl (mapcar (function
             (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
     (prompt (append '("File name: ") tocpl))
     (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (cdr (assoc-ignore-representation fname tocpl)))))
;; ** keybind
;(global-set-key [(control x)(control r)] 'recentf-open-files-compl)
