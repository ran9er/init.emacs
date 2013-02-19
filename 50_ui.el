;; -*- encoding: utf-8-unix; -*-
;; * gui
(if window-system
    ((lambda ()
       (tool-bar-mode 0)                    ;; 禁用工具栏

       ;; 鼠标指针规避光标
       (mouse-avoidance-mode 'exile)
       (setq mouse-avoidance-threshold 10) ;光标靠近该范围,指针规避
       ;; (defun mouse-avoidance-banish-destination()
       ;;   (let* ((pos (window-edges)))
       ;;     (cons (- (nth 2 pos) 2)
       ;;           (nth 1 pos))))


       (defun fname-title-string ()
         "Return the file name of current buffer, using ~ if under home directory"
         (let
             ((fname (or
                      (buffer-file-name (current-buffer))
                      (buffer-name))))
           ;;let body
           (when (string-match (getenv "HOME") fname)
             (setq fname (replace-match "~" t t fname)))
           fname))

       (setq
        ;; 设置标题栏显示文件的完整路径名
        frame-title-format
                        '(:eval (concat (user-login-name) "@" (system-name) "[Emacs"
                         (nth 2 (split-string (version))) "]  " (fname-title-string)))

        ;; fringe 配置
        default-indicate-buffer-boundaries '((top . left) (t . left))

        x-select-enable-clipboard t     ;; 使用X剪贴板
        x-stretch-cursor t              ;; Tab字符使用大光标
        )
)))

;; * 窗口最大化
(add-hook
 'window-setup-hook
 (lambda()
   (if (eq window-system 'w32)
       (w32-send-sys-command 61488)
     (x-send-client-message
      nil 0 nil "_NET_WM_STATE" 32
      '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
     (x-send-client-message
      nil 0 nil "_NET_WM_STATE" 32
      '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))))

;; * filter annoying messages
;+++++++++++++++++++++++++++++++++++++++
(when nil
;+++++++++++++++++++++++++++++++++++++++
(defvar message-filter-regexp-list '("^Starting new Ispell process \\[.+\\] \\.\\.\\.$"
                                     "^Ispell process killed$")
  "filter formatted message string to remove noisy messages")
(defadvice message (around message-filter-by-regexp activate)
  (if (not (ad-get-arg 0))
      ad-do-it
    (let ((formatted-string (apply 'format (ad-get-args 0))))
      (if (and (stringp formatted-string)
               (some (lambda (re) (string-match re formatted-string))
                     message-filter-regexp-list))
          (save-excursion
            (set-buffer "*Messages*")
            (goto-char (point-max))
            (insert formatted-string "\n"))
        (progn
          (ad-set-args 0 `("%s" ,formatted-string))
          ad-do-it)))))
;+++++++++++++++++++++++++++++++++++++++
)
;+++++++++++++++++++++++++++++++++++++++

;; * alpha
(defun alpha (&optional n)
  "range of alpha must be from 0 to 9"
  (interactive "P")
  (if n
      (let ((n (+ (* (mod n 10) 4) 64)))
        (set-frame-parameter (selected-frame) 'alpha n))
    (set-frame-parameter (selected-frame) 'alpha '(100))))
