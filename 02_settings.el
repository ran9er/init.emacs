;; -*- encoding: utf-8-unix; -*-
;; * view
(menu-bar-mode 0)                    ;;禁用菜单栏，F10 开启关闭菜单
(scroll-bar-mode 0)                  ;;禁用滚动栏
(global-font-lock-mode t)            ;;语法加亮
(require 'generic-x)                 ;;增加更多的高亮
(transient-mark-mode t)              ;;高亮显示区域选择

;; * date
(setq 	system-time-locale "C"
        display-time-format "%Y-%m-%d %a %H:%M:%S"
        ;; display-time-day-and-date t
        ;; display-time-24hr-format t
        display-time-interval 1)
(display-time-mode t)                ;;显示时间

;; * 高亮显示成对括号，但不来回弹跳
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; * 使用 y or n 提问
(fset 'yes-or-no-p 'y-or-n-p)

;; * isearch-yank-word-or-char
(defun isearch-yank-word-or-char ()
  ;; default-key: isearch-mode-map C-w
  (interactive)
  (isearch-yank-string
   (if mark-active
       (buffer-substring-no-properties
        (region-beginning) (region-end))
     (current-word nil t)))
  (deactivate-mark))

;; * 不保留备份文件
(setq-default make-backup-files nil)

;; * misc
(setq 
      default-major-mode 'text-mode  ;;缺省模式 text-mode
      default-fill-column 80         ;;页宽 
;     require-final-newline t        ;;以空行结束
      ;;页面上下预留行数，光标进入此范围卷动页面
      scroll-margin 1
      ;;页面平滑卷动，值越大越平滑
      scroll-conservatively 10000
      ;;粘贴于光标处，而不是鼠标指针处
      mouse-yank-at-point t
      ;;只渲染当前屏幕语法高亮，加快显示速度
      font-lock-maximum-decoration t

      ring-bell-function 'ignore        ;;关闭烦人的出错时的提示声
      visible-bell t                    ;;闪屏报警
      column-number-mode t              ;;显示列号
;     resize-mini-windows nil           ;;锁定行高
      enable-recursive-minibuffers nil  ;;递归 minibuffer
      echo-keystrokes 0.1	            ;;按键序列显示延迟，默认 1
      ;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
      ;suggest-key-bindings 1

      ;;设定行距
      default-line-spacing 0
)

;; * 缩进
(setq-default                   ; 使用空格缩进 
        indent-tabs-mode nil    ; t 使用 TAB 作格式化字符  nil 使用空格作格式化字符
        tab-always-indent nil
        tab-width 4)

;; * 作为单词的一部分
(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?[ "w")
(modify-syntax-entry ?] "w")

;; * 让 Emacs 可以直接打开和显示图片。
;(auto-image-file-mode)


;; * 将错误信息显示在回显区
;(condition-case err
;    (progn
;      (require 'xxx) )
;  (error
;   (message "Can't load xxx-mode %s" (cdr err))))
