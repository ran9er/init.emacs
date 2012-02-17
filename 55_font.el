;; -*- encoding: utf-8-unix; -*-
;; * gui
(when window-system

  (setq
   ;; 设置行距
   default-line-spacing   0
   ;; 设置窗口大小 位置
   initial-frame-alist    '((width . 100)(height . 32)(top . 1)(left . 90))
   )

  (setq zhfont (usage-font
                "Yahei Mono"
                "YaHei Consolas Hybrid"
                "Microsoft Yahei"
                "宋体"
                "文泉驿等宽微米黑"
                "文泉驿等宽正黑"
                "文泉驿微米黑")
        btfont (usage-font
                "DejaVu Sans Mono"
                "宋体"
                "SimSun"
                "文泉驿点阵正黑"
                "文泉驿等宽微米黑"
                "文泉驿微米黑")
        enfont (usage-font
                "Proggy"
                "Inconsolata"
                "Consolas"
                "Yahei Mono"
                "YaHei Consolas Hybrid"))

  ;; (set-my-font zhfont 12)
  (set-my-font enfont 12 zhfont)
  ;; (set-my-font zhfont 12 zhfont)
  ;; (set-my-font enfont 14 zhfont 12)


  ;; ** buffer face mode
  (set-my-bf-mode btfont 10)

  (dolist (hook '(
                  completion-list-mode-hook
                  eshell-mode-hook
                  help-mode-hook
                  magit-mode-hook
                  debugger-mode-hook
                  compilation-mode-hook
                  dired-mode-hook
                  vc-mode-line-hook
                  diff-mode-hook
                  ;; ido-make-buffer-list-hook
                  ;; ido-make-file-list-hook
                  ;; ido-make-dir-list-hook
                  ;; undo-tree-visualizer-mode-hook
                  ))
    (add-hook hook 'my-buffer-face-mode))
  ;; ** mode line & head line
  (set-my-ui-font 0.8 btfont)
  )
