;; -*- encoding: utf-8-unix; -*-
(require 'ido)

(ido-mode t)                                        ;开启ido模式
(setq ido-save-directory-list-file "~/.emacs.d/_ido_last")
(setq ido-enable-flex-matching t)                   ;模糊匹配
;; (setq ido-everywhere nil)                           ;禁用ido everyting, 拷贝操作不方便

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map "\C-w" 'ido-delete-backward-updir)
            (define-key ido-completion-map "\C-y" 'ido-copy-current-file-name)
            (define-key ido-completion-map "\C-t" 'ido-restrict-to-matches)))

;; sort ido filelist by mtime instead of alphabetically
;; (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
;; (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
;; (defun ido-sort-mtime ()
;;   (setq ido-temp-list
;;         (sort ido-temp-list
;;               (lambda (a b)
;;                 (time-less-p
;;                  (sixth (file-attributes (concat ido-current-directory b)))
;;                  (sixth (file-attributes (concat ido-current-directory a)))))))
;;   (ido-to-end  ;; move . files to end (again)
;;    (delq nil (mapcar
;;               (lambda (x) (and (char-equal (string-to-char x) ?.) x))
;;               ido-temp-list))))

;; M-x mode
 (defun ido-execute-extended-command ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "Run: "
      (all-completions "" obarray 'commandp)))))
