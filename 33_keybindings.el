;; -*- encoding: utf-8-unix; -*-
(define-key-s 0 '(
    "M-s M-s"   eshell                              ;;
    "C-x C-x"   eshell                              ;; exchange-point-and-mark
    "C-x x"     compile
    "C-x m"     compile                             ;; compose-mail
    "C-x C-j"   ido-execute-extended-command        ;; 
    "C-c C-j"   execute-extended-command            ;; 执行命令 
    "C-o"       set-mark-command                    ;; (open-line) 设置标记
    "C-z"       set-mark-command                    ;; (suspend-frame) 设置标记 
;   "C-x b"     electric-buffer-list                ;; (switch-to-buffer)
    "C-x C-b"   ibuffer                             ;; (list-buffers)
    "M-4"       delete-window                       ;; 关闭当前缓冲区 Alt+4  ;; C-x 0;
    "M-1"       delete-other-windows                ;; 关闭其它缓冲区 Alt+1  ;; C-x 1;
    "M-2"       split-window-vertically             ;; 水平分割缓冲区 Alt+2  ;; C-x 2;
    "M-3"       split-window-horizontally           ;; 垂直分割缓冲区 Alt+3  ;; C-x 3;
    "M-0"       other-window                        ;; 切换到其它缓冲区 Alt+0 ;; C-x o 
    "M-o"       other-window                        ;; 切换到其它缓冲区 Alt+o ;; C-x o 
;   "C-w"       backward-kill-word                  ;; (kill-region)
;   "C-x C-k"   kill-region
;   "C-c C-k"   kill-region
    "C-w"       backward-kill-word-or-kill-region   ;; (kill-region)
    "C-x C-q"   view-mode                           ;; 切换 view-mode
    "M-q"       toggle-read-only                    ;; fill-paragraph
;   "C-x C-j"   execute-extended-command            ;; 执行命令 
    "C-x c"     my-clean-buffer                     ;; 
    "C-x C"     desktop-clear                       ;; 
    "C-x g"     magit-status
    "C-x C-r"   recentf-open-files-compl            ;; 最近打开的文件
    "C-."       undo-tree-visualize
    "C-;"       toggle-comment-region               ;; toggle-comment-region
))




      
;;F10 显示/隐藏菜单栏 ;; M-x menu-bar-open
;;(global-set-key (kbd "F10") 'menu-bar-mode)

;;WIN+s 进入 Shell ;; M-x shell
;(global-set-key (kbd "s-s") 'shell)
;;(define-key ctl-x-map "\M-s" 'shell)
