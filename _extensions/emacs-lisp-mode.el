;; -*- encoding: utf-8-unix; -*-
;; File-name:    <emacs-lisp.el>
;; Create:       <2011-12-24 00:02:38 ran9er>
;; Time-stamp:   <2012-01-17 00:50:59 ran9er>
;; Mail:         <2999am@gmail.com>

(load-once

(tempo-define-snippet "elisp-defun"
  '("(defun " (p "Name: " name) " (" (p "Args" args) ")"
    n> p ")"))


 (when nil
   (require 'snippet)

   (define-abbrev-table 'emacs-lisp-mode-abbrev-table ())

   (snippet-with-abbrev-table
    'emacs-lisp-mode-abbrev-table
    ("defun" . "$>(defun $${name} ($${args})\n$>$.)$>")
    ("defmacro" . "$>(defmacro $${name} ($${args})\n$>$.)$>")
    ("lambda" . "$>(lambda ($${args})($.))$>")
    ("let" . "$>(let ($${args})\n$>($.))$>")
    ("let*" . "$>(let* (($${args}))\n$>($.))$>")
    ("progn" . "$>(progn\n$>($.))$>")
    ("if" . "$>(if ($${cond})\n$>($${true})\n$>$.)$>")
    ("mapc" . "$>(mapc\n$>$${fn}\n$>$.$>)")
    )
   )
 ;; (add-hook 'emacs-lisp-mode-hook
 ;;           (lambda ()
 ;;             (abbrev-mode 1)
 ;;             ;; This line is not in the documentation of snippet.el, but seems to be
 ;;             ;; essential for various modes (not for python-mode though, which serves as
 ;;             ;; the example mode in said documentation)
 ;;             (setq local-abbrev-table emacs-lisp-mode-abbrev-table)))
 )
(when nil
  (abbrev-mode 1)
  (setq local-abbrev-table emacs-lisp-mode-abbrev-table)
)

(if window-system
    ;; (indent-vline-x)
    (progn
      (make-local-variable 'indent-column-list)
      (defvar indent-column-list '(0))
      (indent-vline-s))
  (indent-vline))
