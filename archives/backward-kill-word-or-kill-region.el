;; 智能剪切区域或者单词 (宏)
(defmacro define-2bind-transient-mode (funname cmd-mark-active
                                                   cmd-mark-no-active)
  `(defun ,funname ()
     (interactive)
     (if mark-active
       (call-interactively ,cmd-mark-active)
       (call-interactively ,cmd-mark-no-active))))

(define-2bind-transient-mode
  backward-kill-word-or-kill-region
  'kill-region
  'backward-kill-word)

(global-set-key "\C-w"     'backward-kill-word-or-kill-region)

