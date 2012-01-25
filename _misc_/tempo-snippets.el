;;; tempo-snippets.el --- visual insertion of tempo templates
;;
;; Copyright (C) 2007-2008 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 0.1.5
;; Keywords: abbrev convenience
;; URL: http://nschum.de/src/emacs/tempo-snippets/
;; Compatibility: GNU Emacs 22.2
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; IMPORTANT:
;;
;; Correct use of this package in C-derived (and maybe other) modes depends on
;; the following bug being fixed:
;;
;; http://lists.gnu.org/archive/html/emacs-devel/2007-08/msg00303.html
;;
;; It will only work correctly in Emacs 22.2 and later!
;;
;;
;; Add the following to your .emacs file:
;; (require 'tempo-snippets)
;;
;; Then use `tempo-define-snippet' instead of `tempo-define-template'.  The
;; arguments can remain the same.  Insertion works like for any tempo-template
;; with `tempo-template-your-template-name'.
;;
;; When adding lisp forms in your templates that use `tempo-lookup-named', make
;; sure they don't have side-effects, because they will be evaluated every time
;; the variables change.
;;
;;
;; Here are two examples:
;;
;; (tempo-define-snippet "java-class"
;;   '("class " (p "Class: " class) " {\n\n"
;;     > "public " (s class) "(" p ") {\n" > p n
;;     "}" > n n "}" > n))
;;
;; (tempo-define-snippet "java-get-set"
;;   '("private " (p "Type: " type) " _" (p "Name: " var) ";\n\n"
;;     > "public " (s type) " get" (upcase-initials (tempo-lookup-named 'var))
;;     "() {\n"
;;     > "return _" (s var)  ";\n" "}" > n n
;;     > "public set" (upcase-initials (tempo-lookup-named 'var))
;;     "(" (s type) " value) {\n"
;;     > "_" (s var) " = value;\n" "}" > n))
;;
;; Note the forms in the second example.  It calls `upcase-initials' every time
;; you change the first variable name.
;;
;; You can navigate between input forms with `tempo-snippets-next-field' and
;; `tempo-snippets-previous-field'.  When the point is on an input field, those
;; commands are bound to M-n and M-p by default.  You can use
;; `tempo-snippets-keymap' to bind keys for input fields.
;;
;; If you want to add a snippet to your abbrev table, you can do
;; M-x tempo-snippets-add-mode-abbrev or M-x tempo-snippets-add-global-abbrev.
;;
;;
;;; Change Log:
;;
;; 2008-08-11 (0.1.5)
;;    Added functions for interactive definition of snippet abbrevs.
;;
;; 2008-03-21 (0.1.4)
;;    Added `tempo-snippets-keymap'.
;;
;; 2008-02-27 (0.1.3)
;;    Added support for `tempo-save-named'.
;;
;; 2007-08-23 (0.1.2)
;;    Added `tempo-snippets-complete-tag'.
;;
;; 2007-08-21 (0.1.1)
;;    Fixed documentation.
;;    Prevented crash when form returns nil.
;;    Added `tempo-snippets-grow-in-front' option.
;;    Proper clean-up of `tempo-marks'
;;    Don't jump when first prompt is at point.
;;
;; 2007-08-21 (0.1)
;;    Initial release.
;;
;;; Code:

(require 'tempo)
(eval-when-compile (require 'cl))

(add-to-list 'debug-ignored-errors "^Beginning of buffer$")
(add-to-list 'debug-ignored-errors "^End of buffer$")

(defgroup tempo-snippets nil
  "Visual insertion of tempo templates."
  :group 'abbrev
  :group 'convenience)

(defface tempo-snippets-editable-face
  '((((background dark)) (:background "steel blue"))
    (((background light)) (:background "light cyan")))
  "*Face used for editable text in tempo snippets."
  :group 'tempo-snippets)

(defface tempo-snippets-auto-face
  '((((background dark)) (:underline "steel blue"))
    (((background light)) (:underline "light cyan")))
  "*Face used for automatically updating text in tempo snippets."
  :group 'tempo-snippets)

(defface tempo-snippets-auto-form-face
  '((default (:inherit 'tempo-snippets-auto-face)))
  "*Face used for text in tempo snippets that is re-evaluated on input."
  :group 'tempo-snippets)

(defcustom tempo-snippets-interactive t
  "*Insert prompts for snippets.
If this variable is nil, snippets work just like ordinary tempo-templates with
tempo-interactive set to nil."
  :group 'tempo-snippets
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom tempo-snippets-grow-in-front nil
  "*If this is set, inserting text in front of a field will cause it to grow."
  :group 'tempo-snippets
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defvar tempo-snippets-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\M-n" 'tempo-snippets-next-field)
    (define-key keymap "\M-p" 'tempo-snippets-previous-field)
    keymap)
  "*Keymap used for tempo-nippets input fields.")

;;; tools ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tempo-snippets-overlay-text (overlay)
  (if overlay
      (buffer-substring-no-properties (overlay-start overlay)
                                      (overlay-end overlay))
    ""))

(defun tempo-snippets-set-overlay-text (overlay text)
  (when (overlay-buffer overlay)
    (save-excursion
      (let ((beg (overlay-start overlay))
            (inhibit-modification-hooks t))
        (goto-char beg)
        (delete-char (- (overlay-end overlay) beg))
        (when text
          (insert text))
        (move-overlay overlay beg (point))))))

;;; clearing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun tempo-snippets-clear-all ()
  "Clear all tempo-snippet overlays."
  (interactive)
  (kill-local-variable 'tempo-marks)
  (dolist (s tempo-snippets-sources)
    (tempo-snippets-finish-source s))
  (dolist (f tempo-snippets-forms)
    (delete-overlay f))
  (kill-local-variable 'tempo-snippets-forms))

(defun tempo-snippets-clear (i)
  "Clear a specific snippet."
  (dolist (s tempo-snippets-sources)
    (when (= i (car (overlay-get s 'tempo-snippets-save-name)))
      (tempo-snippets-finish-source s)))
  (dolist (f tempo-snippets-forms)
    (when (= i (overlay-get f 'tempo-snippets-instance-counter))
      (delete-overlay f)
      (setq tempo-snippets-forms (delq f tempo-snippets-forms)))))

;;;###autoload
(defun tempo-snippets-clear-oldest ()
  "Clear the oldest tempo-snippet overlays."
  (interactive)
  (let ((minimum tempo-snippets-instance-counter))
    (dolist (s tempo-snippets-sources)
      (setq minimum (min minimum
                         (car (overlay-get s 'tempo-snippets-save-name)))))
    (tempo-snippets-clear minimum)))

;;;###autoload
(defun tempo-snippets-clear-latest ()
  "Clear the latest tempo-snippet overlays."
  (interactive)
  (let ((maximum 0))
    (dolist (s tempo-snippets-sources)
      (setq maximum (max maximum
                         (car (overlay-get s 'tempo-snippets-save-name)))))
    (tempo-snippets-clear maximum)))

;;; sources ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tempo-snippets-sources nil
  "The list of snippet input fields.")
(make-variable-buffer-local 'tempo-snippets-sources)

(defun tempo-snippets-find-source (save-name &optional instance)
  "Find an input field by name."
  (setq save-name (cons (or instance
                            tempo-snippets-instance-counter)
                        save-name))
  (let ((sources tempo-snippets-sources)
        match)
    (while sources
      (when (equal (overlay-get (car sources) 'tempo-snippets-save-name)
                   save-name)
        (setq match (car sources)
              sources nil))
      (pop sources))
    match))

(defun tempo-snippets-finish-source (overlay)
  "Clear OVERLAY and its mirrors."
  (let ((o (overlay-get overlay 'tempo-snippets-keymap-overlay)))
    (when o (delete-overlay o)))
  (dolist (o (overlay-get overlay 'tempo-snippets-mirrors))
    (delete-overlay o))
  (delete-overlay overlay)
  (setq tempo-snippets-sources
        (delq overlay tempo-snippets-sources)))

(defun tempo-snippets-propagate-source (overlay)
  "Propagate changes to source defined by OVERLAY."
  (let ((text (tempo-snippets-overlay-text overlay))
        (mirrors (overlay-get overlay 'tempo-snippets-mirrors)))
    ;; update mirrors
    (dolist (o mirrors)
      (unless (eq o overlay)
        (tempo-snippets-set-overlay-text o text)))
    ;; update forms
    (tempo-snippets-update-forms)))

;;; forms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tempo-snippets-forms nil
  "The list of automatically re-evaluating snippet forms.")
(make-variable-buffer-local 'tempo-snippets-forms)

(defun tempo-snippets-update-forms ()
  "Re-evaluate all forms."
  (flet ((tempo-lookup-named (name)
            (tempo-snippets-overlay-text
             (tempo-snippets-find-source name))))
    (dolist (ov tempo-snippets-forms)
      (if (overlay-buffer ov)
          (let ((tempo-snippets-instance-counter
                 (overlay-get ov 'tempo-snippets-instance-counter)))
            (tempo-snippets-set-overlay-text
             ov (eval (overlay-get ov 'tempo-snippets-form))))
        (setq tempo-snippets-forms (delq ov tempo-snippets-forms))))))

(defun tempo-snippets-insert-form (form)
  "Insert an automatically re-evaluating snippet form at point."
  (let (overlay eval-result lookup-used)
    ;; FIXME: check for handlers
    (flet ((tempo-lookup-named (name)
              (setq lookup-used t)
              ;; Get value from `tempo-save-named' or snippet source
              (or (cdr (assq name tempo-named-insertions))
                  (tempo-snippets-overlay-text
                   (tempo-snippets-find-source name)))))
      (setq eval-result (eval form)))
    (if lookup-used
      (let ((beg (point)))
        ;; XXX: this assumes on-region to be nil
        (tempo-insert eval-result nil)
        (setq overlay (make-overlay beg (point)))
        (overlay-put overlay 'face 'tempo-snippets-auto-form-face)
        ;; evaporating would cause problems when form before prompt!
        (overlay-put overlay 'tempo-snippets-form form)
        (overlay-put overlay 'modification-hooks
                     '(tempo-snippets-delete-overlay))
        (overlay-put overlay 'insert-in-front-hooks
                     '(tempo-snippets-dont-grow-overlay))
        (overlay-put overlay 'tempo-snippets-instance-counter
                     tempo-snippets-instance-counter)
        (push overlay tempo-snippets-forms)
        "")
      eval-result)))

;;; modification hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tempo-snippets-update (ov after-p beg end &optional r)
  "Called when a snippet input field is modified."
  (when (and after-p (>= beg (overlay-start ov)) (<= beg (overlay-end ov)))
    ;; grow overlay
    (move-overlay ov (overlay-start ov) (max end (overlay-end ov)))
    (tempo-snippets-propagate-source ov)
    (when (= (overlay-end ov) (overlay-start ov))
      (if (> r 1)
          ;; delete overlay and mirrors
          (tempo-snippets-finish-source ov)
        ;; let's be nice and give back a prompt
        (let ((o (overlay-get ov 'tempo-snippets-keymap-overlay)))
          (when o (delete-overlay o)))
        (tempo-snippets-set-overlay-text
         ov (overlay-get ov 'tempo-snippets-prompt))
        (tempo-snippets-propagate-source ov)
        (overlay-put ov 'intangible t)
        (overlay-put ov 'modification-hooks '(tempo-snippets-update))
        (overlay-put ov 'insert-behind-hooks nil)
        (overlay-put ov 'insert-in-front-hooks '(tempo-snippets-replace))))))

(defun tempo-snippets-replace (overlay after-p beg end &optional r)
  "Called when a snippet input prompt is modified."
  (when after-p
    (overlay-put overlay 'intangible nil)
    (overlay-put overlay 'modification-hooks '(tempo-snippets-update))
    (overlay-put overlay 'insert-behind-hooks '(tempo-snippets-update))
    (overlay-put overlay 'insert-in-front-hooks
                 (if tempo-snippets-grow-in-front
                     '(tempo-snippets-update)
                   '(tempo-snippets-dont-grow-overlay)))
    (let ((inhibit-modification-hooks t))
      (delete-region end (overlay-end overlay))
      (tempo-snippets-update overlay t beg end nil))
    ;; keymap overlay for the 1 character behind the input
    (let ((keymap-overlay (make-overlay end (1+ end))))
      (overlay-put keymap-overlay 'evaporate t)
      (overlay-put keymap-overlay 'keymap tempo-snippets-keymap)
      (overlay-put overlay 'tempo-snippets-keymap-overlay keymap-overlay))))

;; Stores removed text for `tempo-snippets-delete-overlay'.
;; We need this, because fontification will call modification hooks, and we want
;; to delete the overlays only on actual text change
(defvar tempo-snippets-delete-overlay-text nil)

(defun tempo-snippets-delete-overlay (ov after-p beg end &optional r)
  "A wrapper to call `delete-overlay' from modification hooks."
  (if after-p
      (unless (string= tempo-snippets-delete-overlay-text
                       (buffer-substring-no-properties beg end))
        (delete-overlay ov))
    (setq tempo-snippets-delete-overlay-text
          (buffer-substring-no-properties beg end))))

(defun tempo-snippets-dont-grow-overlay (ov after-p beg end &optional r)
  "An insert-in-front hook that keeps the original text covered."
  (when after-p
    (move-overlay ov end (overlay-end ov))))

;;; insertions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tempo-snippets-insert-source (prompt save-name)
  "Insert a snippet prompt at point."
  (tempo-insert-mark (point-marker))
  (let ((beg (point))
        (text (replace-regexp-in-string "[[:space:]]" "_"
                                        (if (string-match "\\(.+\\): " prompt)
                                            (match-string 1 prompt)
                                          prompt)))
        overlay)
    (insert text)
    (setq overlay (make-overlay beg (point)))
    (overlay-put overlay 'tempo-snippets-save-name
                 (cons tempo-snippets-instance-counter save-name))
    (overlay-put overlay 'tempo-snippets-prompt text)
    (overlay-put overlay 'face 'tempo-snippets-editable-face)
    (overlay-put overlay 'intangible t)
    (overlay-put overlay 'modification-hooks '(tempo-snippets-update))
    (overlay-put overlay 'insert-in-front-hooks '(tempo-snippets-replace))
    (overlay-put overlay 'tempo-snippets-source t)
    (overlay-put overlay 'keymap tempo-snippets-keymap)
    (push overlay tempo-snippets-sources)
    (tempo-snippets-propagate-source overlay)
    ))

(defun tempo-snippets-insert-mirror (save-name)
  "Insert another instance of a snippet variable at point."
  (let ((saved (cdr (assq name tempo-named-insertions))))
    (if saved
        ;; static saved value found, no need to mirror
        (insert saved)
      (let ((beg (point))
            (source (tempo-snippets-find-source save-name))
            overlay)
        (when source
          (insert (tempo-snippets-overlay-text source))
          (setq overlay (make-overlay beg (point)))
          (let ((mirrors (overlay-get source 'tempo-snippets-mirrors)))
            (push overlay mirrors)
            (overlay-put source 'tempo-snippets-mirrors mirrors))
          (overlay-put overlay 'face 'tempo-snippets-auto-face)
          (overlay-put overlay 'modification-hooks
                       '(tempo-snippets-delete-overlay))
          (overlay-put overlay 'insert-in-front-hooks
                       '(tempo-snippets-dont-grow-overlay)))))))

;;; navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tempo-snippets-source-start-at-point (pos)
  "Return the start of the snippet input field at point."
  (let ((overlays (overlays-in (1- pos) pos)) result)
    (while overlays
      (when (overlay-get (car overlays) 'tempo-snippets-prompt)
        (setq result (car overlays)
              overlays nil))
      (pop overlays))
    (when result
      (overlay-start result))))

;;;###autoload
(defun tempo-snippets-previous-field (&optional arg)
  "Jump to the previous editable tempo-snippet field.
You can also use `tempo-forward-mark', which will include more points of
interest."
  (interactive "p")
  (let ((max-start (point-min))
        (pos (or (tempo-snippets-source-start-at-point (point)) (point)))
        start)
    (dolist (ov tempo-snippets-sources)
      (setq start (overlay-start ov))
      (and (< start pos)
           (> start max-start)
           (setq max-start start)))
    (when (= max-start (point-min))
      (error "Beginning of buffer"))
    (push-mark)
    (goto-char max-start)))

;;;###autoload
(defun tempo-snippets-next-field (&optional arg)
  "Jump to the next editable tempo-snippet field.
You can also use `tempo-backward-mark', which will include more points of
interest."
  (interactive)
  (let ((min-start (point-max))
        (pos (point))
        start)
    (dolist (ov tempo-snippets-sources)
      (setq start (overlay-start ov))
      (and (> start pos)
           (< start min-start)
           (setq min-start start)))
    (when (= min-start (point-max))
      (error "End of buffer"))
    (push-mark)
    (goto-char min-start)))

;;; overridden functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tempo-snippets-insert-prompt (prompt &optional save-name no-insert)
  "`tempo-snippets' version of `tempo-insert-prompt'"
  (if tempo-interactive
      (unless no-insert
        (if (tempo-snippets-find-source save-name)
            (tempo-snippets-insert-mirror save-name)
          (tempo-snippets-insert-source prompt save-name))
        (unless (stringp prompt)
          (error "tempo: The prompt (%s) is not a string" prompt)))
    (tempo-insert-mark (point-marker))))

;;;###autoload
(defun tempo-define-snippet (name elements &optional tag documentation taglist)
  "`tempo-snippets' version of `tempo-define-template'.
Use with the same arguments as `tempo-define-template'.  The resulting template
will prompt for input right in the buffer instead of the minibuffer."
  (let* ((template-name (intern (concat "tempo-template-"
				       name)))
	 (command-name template-name))
    (set template-name elements)
    (fset command-name (list 'lambda (list '&optional 'arg)
			     (or documentation
				 (concat "Insert a " name "."))
			     (list 'interactive "*P")
			     (list 'tempo-snippets-insert-template
                                   (list 'quote template-name)
				   (list 'if 'tempo-insert-region
					 (list 'not 'arg) 'arg))))
    (put command-name 'no-self-insert t)
    (and tag
	 (tempo-add-tag tag template-name taglist))
    command-name))
(put 'tempo-define-snippet 'lisp-indent-function 1)

(defvar tempo-snippets-instance-counter 0
  "Provides unique identifier for each snippet.")

;;;###autoload
(defun tempo-snippets-insert-template (template on-region)
  "`tempo-snippets' version of `tempo-insert-template.'"
  (incf tempo-snippets-instance-counter)
  (let ((tempo-user-elements '((lambda (element)
                                 (tempo-snippets-insert-form element))))
        (tempo-interactive tempo-snippets-interactive)
        (inhibit-modification-hooks t))
    (flet ((tempo-insert-named (name) (tempo-snippets-insert-mirror name))
           (tempo-insert-prompt (a &optional b c)
                                (tempo-snippets-insert-prompt a b c)))
      (if (not tempo-interactive)
          (tempo-insert-template template on-region)
        (save-excursion (tempo-insert-template template on-region))
        (let ((overlays (overlays-at (point)))
              match)
          (while overlays
            (when (overlay-get (pop overlays) 'tempo-snippets-save-name)
              (setq overlays nil
                    match t)))
          (unless match
            (tempo-forward-mark))
          ;; return t so abbrevs don't insert space
          t)))))

;;;###autoload
(defun tempo-snippets-complete-tag (&optional silent)
  "`tempo-snippets' version of `tempo-complete-tag.'"
  ;; unfortunately this is a code clone of the original
  ;; we can't use flet, because that would cause an infinite recursion
  (interactive "*")
  (let* ((collection (tempo-build-collection))
	 (match-info (tempo-find-match-string tempo-match-finder))
	 (match-string (car match-info))
	 (match-start (cdr match-info))
	 (exact (assoc match-string collection))
	 (compl (or (car exact)
		    (and match-info (try-completion match-string collection)))))
    (if compl (delete-region match-start (point)))
    (cond ((null match-info) (or silent (ding)))
	  ((null compl) (or silent (ding)))
	  ((eq compl t) (funcall (cdr (assoc match-string collection))))
	  (t (if (setq exact (assoc compl collection))
		 (funcall (cdr exact))
	       (insert compl)
	       (or silent (ding))
	       (if tempo-show-completion-buffer
		   (tempo-display-completions match-string
					      collection)))))))

;;; convenience commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tempo-snippets-add-mode-abbrev (snippet abbrev)
  "Helper for defining a mode-local abbrev for a snippet."
  (interactive "aTemplate: \nsLocal abbrev: ")
  (define-abbrev local-abbrev-table abbrev "" snippet))

(defun tempo-snippets-add-global-abbrev (snippet abbrev)
  "Helper for defining a mode-local abbrev for a snippet."
  (interactive "aTemplate: \nsGlobal abbrev: ")
  (define-abbrev global-abbrev-table abbrev "" snippet))

(provide 'tempo-snippets)
;;; tempo-snippets.el ends here
