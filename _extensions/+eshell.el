;; -*- encoding: utf-8-unix; -*-
;; don't need load-once, because eshell-load-hook load only once
;; * bash-completion
;+++++++++++++++++++++++++++++++++++++++
(if t t
;+++++++++++++++++++++++++++++++++++++++
(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
  'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
  'bash-completion-dynamic-complete)
;+++++++++++++++++++++++++++++++++++++++
)
;+++++++++++++++++++++++++++++++++++++++

;; * em-smart
;+++++++++++++++++++++++++++++++++++++++
(if t t
;+++++++++++++++++++++++++++++++++++++++
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'after
      eshell-review-quick-commands t
      eshell-smart-displayed t
      eshell-smart-space-goes-to-end t)
;+++++++++++++++++++++++++++++++++++++++
)
;+++++++++++++++++++++++++++++++++++++++

;; * hook
;; C-u M-x eshell 多开
(add-hook 'eshell-mode-hook (lambda()
           (outline-minor-mode 1)
           (eldoc-mode)
;           (lisp-symbol)
           (my-auto-pair)
           (enable-theme 'eshell)
           (eshell-scroll-conservatively)
           (setq
                 pcomplete-cycle-completions   nil
;                 pcomplete-cycle-cutoff-length 4
                 outline-regexp "^[^#$\n]* [#$>]+ "
                 eshell-scroll-to-bottom-on-output t
                 eshell-scroll-show-maximum-output t)
           (add-to-list 'eshell-output-filter-functions
                        'eshell-postoutput-scroll-to-bottom)
            (def-key-s eshell-mode-map
              ;; "C-p"   'eshell-previous-matching-input-from-input
              ;; "C-n"   'eshell-next-matching-input-from-input
              ;; "M-p"   'previous-line
              ;; "M-n"   'next-line
              "<up>"    'eshell-previous-matching-input-from-input
              "<down>"  'eshell-next-matching-input-from-input
              "C-9"     (lambda(&optional x)(interactive "P")(outside "()" 1 " " x))
              "C-8"     'down-list
              "C-7"     '(lambda nil (interactive)(up-list -1))
              )
;          (buffer-face-set 'eshell-custom-face)
))

;; * setting
(setq
      eshell-save-history-on-exit   t
      eshell-history-size           512
      eshell-hist-ignoredups        t
      eshell-cmpl-ignore-case       t
      eshell-cp-interactive-query   t
      eshell-ln-interactive-query   t
      eshell-mv-interactive-query   t
      eshell-rm-interactive-query   t
      eshell-mv-overwrite-files     nil
      ;;  aliases file 中不能有多余的空行，否则报正则表达式错误
      eshell-aliases-file       (expand-file-name "_eshell_/eshell-alias" *init-dir*)
      eshell-highlight-prompt   t
      ;; 提示符设置，两项必须对应起来，否则报 read-only 且不能补全
      eshell-prompt-regexp      "^[^#$\n]* [#$>]+ "
      eshell-prompt-function    (lambda nil
                                  (concat
                                   (abbreviate-file-name
                                    (eshell/pwd))
                                   (if
                                       (=
                                        (user-uid)
                                        0)
                                       " # " " $ ")))
)

(defun eshell-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (mapc (lambda(x)(set (make-local-variable (car x))(cdr x)))
        (alist '(scroll-margin            0
                 scroll-conservatively    10
                 scroll-step              1))))

;; * exntension
(rqx 0
     ;eshell-ls
     eshell-user-key
     eshell-cmpl
     eshell-bmk)

;; * face
;(make-face 'eshell-custom-face)
;(set-face-attribute 'eshell-custom-face nil :font "宋体-10")

;; * func & alias
(defun eshell/ff(file)
  (find-file file))

(defun eshell/img(img)
  (propertize "Image" (quote display) (create-image (expand-file-name img))))

(defun eshell/ee ()
  (find-file (expand-file-name "_extensions/+eshell.el" *init-dir*)))

(defun eshell/aa ()
  (find-file eshell-aliases-file))

(defun eshell/rr ()
  (find-file (expand-file-name "_qref.org" work-dir)))

(defun eshell/ed (file1 file2)
  (ediff-files file1 file2))

;; ** alternate func
(defun eshell/less (&rest args)
  "Invoke `view-file' on a file. "less +42 foo" will go to line 42 in
    the buffer for foo."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (tyler-eshell-view-file file)
          (goto-line line))
      (tyler-eshell-view-file (pop args)))))
(defalias 'eshell/more 'eshell/less)

;; * last command timer
(add-hook 'eshell-load-hook
          (lambda()(setq last-command-start-time (float-time))))
(add-hook 'eshell-pre-command-hook
          (lambda()(setq last-command-start-time (float-time))))
(add-hook 'eshell-before-prompt-hook
          (lambda()
              (message "%s ==> spend %g seconds"
                       (cond
                        ((not eshell-last-command-name) "GO!")
                        ((string-match "^#<" eshell-last-command-name)
                         (substring eshell-last-command-name 2 -1))
                        (t eshell-last-command-name))
                       (- (float-time) last-command-start-time))))

;; * ac-mode
;+++++++++++++++++++++++++++++++++++++++
(if t t
;+++++++++++++++++++++++++++++++++++++++
(defvar ac-source-eshell-pcomplete
  '((candidates . (pcomplete-completions))))
(defun ac-complete-eshell-pcomplete ()
  (interactive)
  (auto-complete '(ac-source-eshell-pcomplete)))
;; (ac-define-source eshell-pcomplete
;;   '((candidates . (pcomplete-completions))
;;     (cache)))
;; (push 'ac-complete-eshell-pcomplete ac-trigger-commands)

(add-to-list 'ac-modes 'eshell-mode)
(setq ac-sources '(;; ac-source-semantic
                   ;; ac-source-yasnippet
                   ac-source-eshell-pcomplete
                   ;; ac-source-files-in-current-dir
                   ;; ac-source-filename
                   ;; ac-source-abbrev
                   ;; ac-source-words-in-buffer
                   ;; ac-source-words-in-all-buffer
                   ;; ac-source-symbols
                   ;; ac-source-imenu
))
;+++++++++++++++++++++++++++++++++++++++
)
;+++++++++++++++++++++++++++++++++++++++


;; * other func
(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(defun eshell/vi (&rest args)
  "Invoke `find-file' on the file.
\"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))

(defun eshell/emacs (&rest args)
  "Open a file in emacs. Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

;; * export loaded times
(let ((var (read (concat "*load--" load-file-name))))
  (eval `(defvar ,var 0))
  (eval `(setq ,var (1+ ,var))))
