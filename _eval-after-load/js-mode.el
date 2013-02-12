;; -*- encoding: utf-8-unix; -*-
;; File-name:    <73_js2.el>
;; Create:       <2011-12-14 10:23:27 ran9er>
;; Time-stamp:   <2012-01-03 15:09:52 ran9er>
;; Mail:         <2999am@gmail.com>

;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;; you can;t have a symbol called "someName:false"

;; (require 'js2-mode)
;; (add-hook 'js2-post-parse-callbacks 
;;           (lambda ()
;;             (let ((btext (replace-regexp-in-string
;;                           ": *true" " "
;;                           (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
;;               (setq js2-additional-externs
;;                     (split-string
;;                      (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
;;                      " *, *" t))
;;               )))
(load-once
 (require 'snippet)

 (define-abbrev-table 'javascript-mode-abbrev-table ())

 (snippet-with-abbrev-table
  'javascript-mode-abbrev-table
  ("for" .  "$>for (var $${i}=$${0},$${len}=$${i}.length;$${i}<$${len};++$${i}){\n$>$.\n}$>")
  ("forin" . "$>for (var $${i} in $${var}){\n$>$$;\n}$.$>")
  ("if"  .  "$>if ($${cond}){$>\n$>$.;\n}$>")
  ("ifel" . "$>if ($${cond}){$>\n$>$$;\n} else {$>\n$>$$;\n}$.$>")
  ("wh"  .  "$>while ($${i}){\n$>$.\n}$>")
  ("whinc" . "$>while ($${i}<$${10}){\n$>$.\n$>$${i}++;\n}$>")
  ("trn" . "$${if}?$${then}:$${else}")
  ("var" . "var $${variable} = $${value};")
  ("fun" . "$>function $${name}($${args}){\n$>$.\n}$>")
  ("lambda" . "$>function ($${args}){\n$>$.\n}$>")
  ("df" . "document.forms['$${formname}']")
  ("cl" . "console.log('$${message}');")) ;Firebug logging 

 (add-hook 'javascript-mode-hook
           (lambda ()
             (abbrev-mode 1)
             ;; This line is not in the documentation of snippet.el, but seems to be
             ;; essential for various modes (not for python-mode though, which serves as
             ;; the example mode in said documentation)
             (setq local-abbrev-table javascript-mode-abbrev-table)))
 )
