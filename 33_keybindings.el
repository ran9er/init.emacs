;; -*- encoding: utf-8-unix; -*-
(define-key-s 0 '(
    ;; "C-x b"     anything                            ;; (switch-to-buffer)
    [remap list-buffers]
                ibuffer                             ;; (list-buffers) "C-x C-b"
    [remap newline]
                newline-and-indent
    [remap kill-region]
                smart-backward-kill                 ;; (kill-region) "C-w"
    "C-x C-x"   eshell                              ;; exchange-point-and-mark
    "C-x x"     compile
    "C-x m"     compile                             ;; compose-mail
    "C-x C-j"   ido-execute-extended-command        ;;
    "C-c C-j"   execute-extended-command            ;; 执行命令
    "C-o"       set-mark-command                    ;; (open-line) 设置标记
    "C-z"       set-mark-command                    ;; (suspend-frame) 设置标记
    "M-4"       delete-window                       ;; 关闭当前缓冲区 Alt+4  ;; C-x 0;
    "M-1"       delete-other-windows                ;; 关闭其它缓冲区 Alt+1  ;; C-x 1;
    "M-2"       split-window-vertically             ;; 水平分割缓冲区 Alt+2  ;; C-x 2;
    "M-3"       split-window-horizontally           ;; 垂直分割缓冲区 Alt+3  ;; C-x 3;
    "M-0"       other-window                        ;; 切换到其它缓冲区 Alt+0 ;; C-x o
    "M-o"       other-window                        ;; 切换到其它缓冲区 Alt+o ;; C-x o
    ;; "M-\\"      resize-horizontal-space             ;; delete-horizontal-space
    "C-x C-q"   view-mode                           ;; 切换 view-mode
    "M-q"       toggle-read-only                    ;; fill-paragraph
    "M-t"       swap-point                          ;; transpose-words
    "C-x c"     my-clean-buffer                     ;;
    "C-x C"     desktop-clear                       ;;
    "C-x g"     magit-status
    "C-x C-r"   recentf-open-files-compl            ;; 最近打开的文件
    "C-."       undo-tree-visualize
    "C-;"       comment-or-uncomment-region         ;; toggle-comment-region
    "C-x i"     expand-abbrev                       ;; ido-insert-file
    "C-x TAB"   expand-abbrev                       ;; indent-rigidly
    "<f5>"      run-current-file
    "<f6>"      run-current-file
    "C-x f"     find-temp
    "C-x w"     write-temp
    "<C-return>" temp-func-call
    "<M-return>" temp-func-add
    ;; "C-x f"     (lambda()                                ;; set-fill-column
    ;;               (interactive)
    ;;               (let ((p (point)))
    ;;                 (find-alternate-file (buffer-file-name))
    ;;                 (goto-char p)))
))

(def-keys-low
 ()
 ?\C-m  'eshell
 ?\C-i  'undo-tree-visualize
 ?\C-\[  'magit-status)

;;F10 显示/隐藏菜单栏 ;; M-x menu-bar-open
;;(global-set-key (kbd "F10") 'menu-bar-mode)

;;WIN+s 进入 Shell ;; M-x shell
;(global-set-key (kbd "s-s") 'shell)
;;(define-key ctl-x-map "\M-s" 'shell)
