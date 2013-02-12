;; -*- encoding: utf-8-unix; -*-
;;;; 小说阅读
;; * define several class of keywords
(defvar novel-keywords
  '("break" "default" "do" "else" "for" "if" "return" "state" "while")
  "NVL keywords.")

(defvar novel-types
  '("float" "integer" "key" "list" "rotation" "string" "vector")
  "NVL types.")

(defvar novel-constants
  '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK")
  "NVL constants.")

(defvar novel-events
  '("at_rot_target" "at_target" "attach")
  "NVL events.")

(defvar novel-functions
  '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList")
  "NVL functions.")

;; * create the regex string for each class of keywords
(defvar novel-keywords-regexp (regexp-opt novel-keywords 'words))
(defvar novel-type-regexp (regexp-opt novel-types 'words))
(defvar novel-constant-regexp (regexp-opt novel-constants 'words))
(defvar novel-event-regexp (regexp-opt novel-events 'words))
(defvar novel-functions-regexp (regexp-opt novel-functions 'words))

;; * clear memory
(setq novel-keywords nil)
(setq novel-types nil)
(setq novel-constants nil)
(setq novel-events nil)
(setq novel-functions nil)

;; * create the list for font-lock.
;; each class of keyword is given a particular face
(setq novel-font-lock-keywords
  `(
    (,novel-type-regexp . font-lock-type-face)
    (,novel-constant-regexp . font-lock-constant-face)
    (,novel-event-regexp . font-lock-builtin-face)
    (,novel-functions-regexp . font-lock-function-name-face)
    (,novel-keywords-regexp . font-lock-keyword-face)
    ;; note: order above matters. “novel-keywords-regexp” goes last because
    ;; otherwise the keyword “state” in the function “state_entry”
    ;; would be highlighted.
))

;; * define the mode
(define-derived-mode novel-mode text-mode "Novel"
  "xiaoshou mode"
  (setq font-lock-defaults '((novel-font-lock-keywords)))

  ;; code for syntax highlighting
  (setq font-lock-defaults '((novel-font-lock-keywords)))

  ;; clear memory
  (setq novel-keywords-regexp nil)
  (setq novel-types-regexp nil)
  (setq novel-constants-regexp nil)
  (setq novel-events-regexp nil)
  (setq novel-functions-regexp nil)

  (view-mode)
  (setq outline-regexp "第.*章")
  (outline-minor-mode)
  (hide-body)
  (local-set-key [(mouse-3)] 'View-scroll-page-forward)
)

