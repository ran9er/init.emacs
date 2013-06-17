(if (eq system-type 'windows-nt)
    (setq mew-path (expand-file-name "../../mew/" exec-directory)))
(add-to-list 'load-path mew-path)
(setq mew-file (expand-file-name "mew.el" mew-path))
(setq mew-icon-directory (expand-file-name "etc" mew-path))
(add-exec-path (expand-file-name "bin" mew-path))

;;;###autoload
(autoload 'mew mew-file nil t)
;;;###autoload
(autoload 'mew-send mew-file nil t)
(if (boundp 'read-mail-command)
    (setq read-mail-command 'mew))
;;;###autoload
(autoload 'mew-user-agent-compose mew-file nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
       'mew-user-agent
       'mew-user-agent-compose
       'mew-draft-send-message
       'mew-draft-kill
       'mew-send-hook))

(setq mew-pop-size 0)
(setq mew-smtp-auth-list nil)
(setq toolbar-mail-reader 'Mew)
(set-default 'mew-decode-quoted 't)
(setq mew-prog-pgp "gpg")
(setq mew-smtp-auth-list nil)
(setq mew-pop-auth 'pass) ;;认证方式
(setq mew-use-cached-passwd t)
(setq mew-summary-form
      '(type (5 date) " " (14 from) " " t (0 subj)))
(setq mew-summary-form-extract-rule '(name))

;; (when (boundp 'utf-translate-cjk)
;;       (setq utf-translate-cjk t)
;;       (custom-set-variables
;;          '(utf-translate-cjk t)))
;; (if (fboundp 'utf-translate-cjk-mode)
;;     (utf-translate-cjk-mode 1))
(require 'flyspell) ;;非常好用的英文的拼写检查
(load-file (expand-file-name "myconf.el" mew-path))
;; (setq mew-config-alist
;;       '(
;;         (default
;;           (name                  "Name")
;;           (mailbox-type          imap)
;;           (proto                 "%")
;;           (imap-server           "imap.gmail.com")
;;           (imap-user             "user")
;;           (imap-size             0)
;;           (imap-delete           t)
;;           (imap-queue-folder     "%queue")
;;           (imap-trash-folder     "%Trash") ;; This must be in concile with your IMAP box setup
;;           (mail-domain           "gmail.com")
;;           (user                  "user")
;;           (smtp-auth-list        ("PLAIN" "LOGIN" "CRAM-MD5"))
;;           (smtp-user             "user")
;;           (smtp-server           "smtp.gmail.com"))
;;         ))

