;; -*- encoding: utf-8-unix; -*-
;; * autoload
(mapc 'load (directory-files (expand-file-name "_autoload/" *init-dir*) t "\\.el\\'"))

;; * auto-hooks
(let* ((dir (expand-file-name "_extensions/" *init-dir*))
       (ext (mapcar
             (lambda(x)(cons (file-name-sans-extension (file-name-nondirectory x)) x))
             (directory-files dir t "\\.el\\'"))))
  (add-hook 'find-file-hook
            `(lambda ()
               (let (mode)
                 (mapcar (lambda(x)
                           (and
                            (string-match (car x)(buffer-name))
                            (setq mode (cdr x))))
                         auto-mode-alist)
                 (setq mode
                       (or mode
                           (and (string-equal "*" (substring (buffer-name) 0 1))
                                (substring (buffer-name) 1 -1))))
                 (load (or
                        (cdr (assoc (symbol-name mode) ',ext))
                        (make-temp-name ""))
                       t)))))

;; * environment
(if (eq system-type 'windows-nt)
    (mapc (lambda (p)(add-exec-path p))
          (list
           (expand-file-name "../../git/bin/" exec-directory)
           (expand-file-name "../other/sdcv/" exec-directory)
           exec-directory)))

;; * working dir
(setq work-dir (expand-file-name "sandbox/" *init-dir*))
(cd work-dir)

;; * time-stamp-format
;(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")

;; * 打开文件
(add-hook 'find-file-hook
          '(lambda ()
             (if (file-exists-p (buffer-file-name))             ; 已存在文件
                 (if (member (file-name-extension (buffer-name))
                             '("el" "bak" "txt"))               ; 匹配扩展名
                     (view-mode))                               ; 启用 view-mode
               ;; (insert "-*- encoding: utf-8-unix; -*-")         ; 插入文件变量
               ;; (comment-region 0 (point-max))
               ;; (end-of-line)(newline)
               (insert-doc-head)
             )))

;; * 保存文件
(add-hook 'before-save-hook
          '(lambda()
             (if (and (eolp)
                      (equal (char-before) 32))
                 (progn
                   ;; (del-tail-spc)
                   (delete-trailing-whitespace)
                   (untabify (point-min) (point-max))))
             (time-stamp)))

;; * lisp mode
(mapc (lambda (mode)
        (add-hook
         (concat-symbol mode '-hook)
         `(lambda ()
            (lisp-symbol)
            (eldoc-mode)
            (def-key-s ,(concat-symbol mode '-map)
              "C-9"       (outside "()" 1 " ")
              "C-8"       'down-list
              "C-7"       '(lambda nil (interactive)(up-list -1))
              ))))
      '(lisp-mode
        lisp-interaction-mode
        emacs-lisp-mode))

;; * max
(setq max-lisp-eval-depth   1000        ;lisp最大执行深度   500
      max-specpdl-size      10000       ;最大容量           1000
      kill-ring-max         1024        ;kill ring          60
      undo-outer-limit      5000000     ;撤销限制           12000000
      mark-ring-max         1024        ;mark ring          16
)

;; * Common
(setq message-log-max         t        ;完整的 message-log
      inhibit-startup-message t        ;禁用启动画面
      initial-scratch-message          ;初始内容
      (purecopy "\
;; In sandbox
"))

;; * coding-system
;; 语言环境
;(set-language-environment 'utf-8)
;(set-clipboard-coding-system 'utf-8)            ;; 剪切板，用于和其他程序之间复制内容
(setq default-buffer-file-coding-system  'utf-8-unix)     ; 文件
;; (when (eq system-type 'windows-nt)
;;   (setq
;;       default-file-name-coding-system   'chinese-iso-8bit ; 文件名
;;       default-keyboard-coding-system    'chinese-iso-8bit ; 键盘输入，用于输入法。
;;       default-terminal-coding-system    'chinese-iso-8bit ; 终端显示的编码方式。
;;       ))

;; * safe-local-variable
(setq safe-local-variable-values '(
      (encoding . utf-8)
      (encoding . utf-8-unix)
      (encoding . utf-8-emacs-unix)
))

;; * help
(setq help-xref-following nil)

;; * 开启服务器模式
;(server-start)
