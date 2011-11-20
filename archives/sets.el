;; In sandbox
(which-function-mode t)                 ;在状态条上显示当前光标在哪个函数体内部
(auto-compression-mode 1)               ;打开压缩文件时自动解压缩

;;; ### Doxymacs ###
;;; --- 注释管理
(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               ))
  (doxymacs-font-lock)                                                        ;注释高亮模式
  (add-hook hook 'doxymacs-mode)                                              ;加载文档模式
  (add-hook hook (lambda () (local-set-key (kbd "C-m") 'my-doxymacs-return))) ;注释智能换行
  )

;;; ### Ibuffer ###
;;; --- 交互式Buffer
(setq ibuffer-sorting-mode 'recency)    ;用最近打开模式显示

;;; ### Auto-fill ###
;;; --- 自动换行
(setq default-fill-column 100)          ;默认显示 100列就换行
(dolist (hook (list
               'after-text-mode-hook
               'message-mode-hook
               'org-mode-hook
               ))
  (add-hook hook '(lambda () (auto-fill-mode 1))))