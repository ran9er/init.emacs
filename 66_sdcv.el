;; -*- encoding: utf-8-unix; -*-
(if (eq system-type 'windows-nt)
    (setq sdcv-cmd (concat "sdcv " "--data-dir "
                           (expand-file-name "../other/sdcv/dict/" exec-directory))))

(global-set-key (kbd "C-c d") 'sdcv-to-buffer)
