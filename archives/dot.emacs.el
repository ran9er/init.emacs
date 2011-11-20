;;========================================
;;添加 Emacs 搜索目录 可以将自定的扩展放该目录
;;========================================
;(add-to-list 'load-path "~/..emacs")
;如果有其它配置文件，使此命令读取
;(load "addon.el")
	  
;;========================================
;; 外观设置
;;========================================

;;禁用工具栏
(tool-bar-mode nil)

;;禁用菜单栏，F10 开启关闭菜单
(menu-bar-mode nil)

;;禁用滚动栏，用鼠标滚轮代替
;;(scroll-bar-mode nil)

;;禁用启动画面
(setq inhibit-startup-message t)

;;========================================
;; 键绑定
;;========================================

;; C-t 设置标记 ;; 
(global-set-key (kbd "C-t") 'set-mark-command)

;; C-x b => CRM bufer list
(global-set-key "\C-xb" 'electric-buffer-list)

;;---------- redo
(global-set-key ( kbd "C-.") 'redo)

;;========================================
;;关闭当前缓冲区 Alt+4  ;; C-x 0
(global-set-key (kbd "M-4") 'delete-window)
;;关闭其它缓冲区 Alt+1  ;; C-x 1
(global-set-key (kbd "M-1") 'delete-other-windows)
;;水平分割缓冲区 Alt+2  ;; C-x 2
(global-set-key (kbd "M-2") 'split-window-vertically)
;;垂直分割缓冲区 Alt+3  ;; C-x 3
(global-set-key (kbd "M-3") 'split-window-horizontally)
;;切换到其它缓冲区 Alt+0 ;; C-x o 
(global-set-key (kbd "M-0") 'other-window)
	  
	  
;;F10 显示/隐藏菜单栏 ;; M-x menu-bar-open
;;(global-set-key (kbd "F10") 'menu-bar-mode)

;;WIN+s 进入 Shell ;; M-x shell
;(global-set-key (kbd "s-s") 'shell)
;;(define-key ctl-x-map "\M-s" 'shell)


;;========================================
;; 缓冲区
;;========================================

;;设定行距
(setq default-line-spacing 0)

;;页宽 
(setq default-fill-column 90)

;;缺省模式 text-mode
(setq default-major-mode 'text-mode)

;;设置删除纪录
(setq kill-ring-max 200)

;;以空行结束
(setq require-final-newline t) 


;;语法加亮
(global-font-lock-mode t)

;;高亮显示区域选择
(transient-mark-mode t)

;;页面平滑滚动， scroll-margin 5 靠近屏幕边沿3行时开始滚动，可以很好的看到上下文。
(setq scroll-margin 5
      scroll-conservatively 10000)

;高亮显示成对括号，但不来回弹跳
(show-paren-mode t)
(setq show-paren-style 'parentheses)


;;鼠标指针规避光标
;(mouse-avoidance-mode 'animate)

;;粘贴于光标处，而不是鼠标指针处
(setq mouse-yank-at-point t)

;;========================================
;; 回显区
;;========================================

;;闪屏报警
(setq visible-bell t)

;;使用 y or n 提问
(fset 'yes-or-no-p 'y-or-n-p)

;;锁定行高
(setq resize-mini-windows nil)

;;递归 minibuffer
(setq enable-recursive-minibuffers t)

;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
;;(setq suggest-key-bindings 1) ;;

;;========================================
;; 状态栏
;;========================================

;;显示时间
(display-time)
;;时间格式
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)

;;显示列号
(setq column-number-mode t)

;;标题栏显示 %f 缓冲区完整路径 %p 页面百分数 %l 行号
(setq frame-title-format "%f")


;;========================================
;; 编辑器设定
;;========================================

;;不生成临时文件
;;(setq-default make-backup-files nil)

;;只渲染当前屏幕语法高亮，加快显示速度
(setq font-lock-maximum-decoration t)

;;将错误信息显示在回显区
;(condition-case err
;    (progn
;    (require 'xxx) )
;  (error
;   (message "Can't load xxx-mode %s" (cdr err))))

;;使用X剪贴板
(setq x-select-enable-clipboard t) 
;;;;;;;; 使用空格缩进 ;;;;;;;;
;; indent-tabs-mode  t 使用 TAB 作格式化字符  nil 使用空格作格式化字符
(setq indent-tabs-mode nil)
(setq tab-always-indent nil)
(setq tab-width 4)

;;========================================
;; 颜色设置
;;========================================

;; 指针颜色
(set-cursor-color "black")
;; 鼠标颜色
(set-mouse-color "black")
;; 背景和字体颜色
(set-foreground-color "gainsboro")
(set-background-color "grey30")
(set-border-color "black")
;; 语法高亮显示，区域选择，二次选择 ;;前景和背景色
(set-face-foreground 'highlight "white")
(set-face-background 'highlight "blue")
(set-face-foreground 'region "cyan")
(set-face-background 'region "blue")
(set-face-foreground 'secondary-selection "skyblue")
(set-face-background 'secondary-selection "darkblue")

;;日历配色
;(setq calendar-load-hook
;'(lambda ()
;(set-face-foreground 'diary-face "skyblue")
;(set-face-background 'holiday-face "slate blue")
;(set-face-foreground 'holiday-face "white")))

;;========================================
;; 字体设置
;;========================================
(set-default-font "Monospace-10")
(if window-system
   (set-fontset-font (frame-parameter nil 'font)
	  'unicode '("Microsoft YaHei" . "unicode-bmp"))
)


;;========================================
;; 必备扩展 
;;========================================


;;---------- color-theme
;(require 'color-theme)
;(color-theme-gray30)

;;========== ibuffer
(require 'ibuffer)
(global-set-key ( kbd "C-x C-b ")' ibuffer)

;;========== outline
(setq outline-minor-mode-prefix [(control o)])

;;---------- Docbook
;(require 'docbook-xml-mode)

;(add-hook 'docbook-xml-mode-hook
;	  (function (lambda ()
;                  (setq outline-regexp "<!\\-\\-\\*+")
;		      (outline-minor-mode)
;		      (hide-body)
;	    		)))


;;开启服务器模式
;(server-start)

;;org-mode
(setq org-hide-leading-stars t)
 (define-key global-map "\C-ca" 'org-agenda)
 (setq org-log-done 'time)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; F5 运行当前文件 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun run-current-file ()
;  "Execute or compile the current file.
;For example, if the current buffer is the file x.pl,
;then it'll call “perl x.pl” in a shell.
;The file can be php, perl, python, bash, java.
;File suffix is used to determine what program to run."
;(interactive)
;  (let (ext-map file-name file-ext prog-name cmd-str)
;; get the file name
;; get the program name
;; run it
;    (setq ext-map
;          '(
;            ("php" . "php")
;            ("pl" . "perl")
;            ("py" . "python")
;            ("sh" . "bash")
;            ("java" . "javac")
;            )
;          )
;    (setq file-name (buffer-file-name))
;    (setq file-ext (file-name-extension file-name))
;    (setq prog-name (cdr (assoc file-ext ext-map)))
;    (setq cmd-str (concat prog-name " " file-name))
;;    (compile cmd-str)))
;    (shell-command cmd-str)))
;
;(global-set-key (kbd "<f5>") 'run-current-file)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; 以下为实现 redo 的代码 ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'redo)

(defvar redo-version "1.02"
  "Version number for the Redo package.")

(defvar last-buffer-undo-list nil
  "The head of buffer-undo-list at the last time an undo or redo was done.")
(make-variable-buffer-local 'last-buffer-undo-list)

(make-variable-buffer-local 'pending-undo-list)

;; Emacs 20 variable
(defvar undo-in-progress)

(defun redo (&optional count)
  "Redo the the most recent undo.
Prefix arg COUNT means redo the COUNT most recent undos.
If you have modified the buffer since the last redo or undo,
then you cannot redo any undos before then."
  (interactive "*p")
  (if (eq buffer-undo-list t)
      (error "No undo information in this buffer"))
  (if (eq last-buffer-undo-list nil)
      (error "No undos to redo"))
  (or (eq last-buffer-undo-list buffer-undo-list)
      ;; skip one undo boundary and all point setting commands up
      ;; until the next undo boundary and try again.
      (let ((p buffer-undo-list))
	(and (null (car-safe p)) (setq p (cdr-safe p)))
	(while (and p (integerp (car-safe p)))
	  (setq p (cdr-safe p)))
	(eq last-buffer-undo-list p))
      (error "Buffer modified since last undo/redo, cannot redo"))
  (and (or (eq buffer-undo-list pending-undo-list)
	   (eq (cdr buffer-undo-list) pending-undo-list))
       (error "No further undos to redo in this buffer"))
  (or (eq (selected-window) (minibuffer-window))
      (message "Redo..."))
  (let ((modified (buffer-modified-p))
	(undo-in-progress t)
	(recent-save (recent-auto-save-p))
	(old-undo-list buffer-undo-list)
	(p (cdr buffer-undo-list))
	(records-between 0))
    ;; count the number of undo records between the head of the
    ;; undo chain and the pointer to the next change.  Note that
    ;; by `record' we mean clumps of change records, not the
    ;; boundary records.  The number of records will always be a
    ;; multiple of 2, because an undo moves the pending pointer
    ;; forward one record and prepend a record to the head of the
    ;; chain.  Thus the separation always increases by two.  When
    ;; we decrease it we will decrease it by a multiple of 2
    ;; also.
    (while p
      (cond ((eq p pending-undo-list)
	     (setq p nil))
	    ((null (car p))
	     (setq records-between (1+ records-between))
	     (setq p (cdr p)))
	    (t
	     (setq p (cdr p)))))
    ;; we're off by one if pending pointer is nil, because there
    ;; was no boundary record in front of it to count.
    (and (null pending-undo-list)
	 (setq records-between (1+ records-between)))
    ;; don't allow the user to redo more undos than exist.
    ;; only half the records between the list head and the pending
    ;; pointer are undos that are a part of this command chain.
    (setq count (min (/ records-between 2) count)
	  p (primitive-undo (1+ count) buffer-undo-list))
    (if (eq p old-undo-list)
            nil ;; nothing happened
      ;; set buffer-undo-list to the new undo list.  if has been
      ;; shortened by `count' records.
      (setq buffer-undo-list p)
      ;; primitive-undo returns a list without a leading undo
      ;; boundary.  add one.
      (undo-boundary)
      ;; now move the pending pointer backward in the undo list
      ;; to reflect the redo.  sure would be nice if this list
      ;; were doubly linked, but no... so we have to run down the
      ;; list from the head and stop at the right place.
      (let ((n (- records-between count)))
	(setq p (cdr old-undo-list))
	(while (and p (> n 0))
	  (if (null (car p))
	      (setq n (1- n)))
	  (setq p (cdr p)))
	(setq pending-undo-list p)))
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary recent-save))
    (or (eq (selected-window) (minibuffer-window))
	(message "Redo!"))
    (setq last-buffer-undo-list buffer-undo-list)))

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  (interactive "*p")
  (let ((modified (buffer-modified-p))
	(recent-save (recent-auto-save-p)))
    (or (eq (selected-window) (minibuffer-window))
	(message "Undo..."))
    (or (eq last-buffer-undo-list buffer-undo-list)
	;; skip one undo boundary and all point setting commands up
	;; until the next undo boundary and try again.
	(let ((p buffer-undo-list))
	  (and (null (car-safe p)) (setq p (cdr-safe p)))
	  (while (and p (integerp (car-safe p)))
	    (setq p (cdr-safe p)))
	  (eq last-buffer-undo-list p))
	(progn (undo-start)
	       (undo-more 1)))
    (undo-more (or arg 1))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
    ;;
    ;;;; The old code for this was mad!  It deleted all set-point
    ;;;; references to the position from the whole undo list,
    ;;;; instead of just the cells from the beginning to the next
    ;;;; undo boundary.  This does what I think the other code
    ;;;; meant to do.
    (let ((list buffer-undo-list)
    	  (prev nil))
      (while (and list (not (null (car list))))
    	(if (integerp (car list))
    	    (if prev
    		(setcdr prev (cdr list))
    	      ;; impossible now, but maybe not in the future 
    	      (setq buffer-undo-list (cdr list))))
    	(setq prev list
    	      list (cdr list))))
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary recent-save)))
  (or (eq (selected-window) (minibuffer-window))
      (message "Undo!"))
  (setq last-buffer-undo-list buffer-undo-list))
