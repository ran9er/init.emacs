;; -*- encoding: utf-8-unix; -*-
;; File-name:    <67_emms.el>
;; Create:       <2012-09-02 12:21:54 ran9er>
;; Time-stamp:   <2012-09-02 13:20:49 ran9er>
;; Mail:         <2999am@gmail.com>
(add-to-list 'load-path (expand-file-name "emms" *init-dir*))
;;; Track Show Format (for playlist buffer)
(autoload 'emms-play-playlist "emms-source-playlist" nil t)
(global-set-key (kbd "C-c e l") 'emms-play-playlist)

(eval-after-load "emms"
  '(progn
     (require 'emms-setup)
     (emms-standard)
     ;; no cli volume setup tools in windows
                                        ;(require 'emms-volume)
     (require 'emms-score)
     (emms-score 1)
     ;; autodetect musci files id3 tags encodeing
     (require 'emms-i18n)
     ;; auto-save and import last playlist
     (require 'emms-history)

     (setq emms-player-mpg321-command-name "mpg123"
           emms-player-mplayer-command-name "mplayer"
           emms-player-list '(emms-player-mplayer
                              emms-player-mplayer-playlist
                              emms-player-ogg123
                              emms-player-mpg321))

     ;; my customizable playlist format
     (defun bigclean-emms-info-track-description (track)
       "Return a description of the current track."
       (let ((artist (emms-track-get track 'info-artist))
             (title (emms-track-get track 'info-title))
             (album (emms-track-get track 'info-album))
             (ptime (emms-track-get track 'info-playing-time)))
         (if title
             (format
              "%-35s %-40s %-35s %5s:%-5s"
              (if artist artist "")
              (if title title "")
              (if album album "")
              (/ ptime 60)
              (% ptime 60)))))
     (setq emms-track-description-function
           'bigclean-emms-info-track-description)

     ;; format current track,only display title in mode line
     (defun bigclean-emms-mode-line-playlist-current ()
       "Return a description of the current track."
       (let* ((track (emms-playlist-current-selected-track))
              (type (emms-track-type track))
              (title (emms-track-get track 'info-title)))
         (format "[ %s ]"
                 (cond ((and title)
                        title)))))
     (setq emms-mode-line-mode-line-function
           'bigclean-emms-mode-line-playlist-current)

     ;; global key-map
     ;; all global keys prefix is C-c e
     ;; compatible with emms-playlist mode keybindings
     ;; you can view emms-playlist-mode.el to get details about
     ;; emms-playlist mode keys map
     (global-set-key (kbd "C-c e s") 'emms-stop)
     (global-set-key (kbd "C-c e P") 'emms-pause)
     (global-set-key (kbd "C-c e n") 'emms-next)
     (global-set-key (kbd "C-c e p") 'emms-previous)
     (global-set-key (kbd "C-c e f") 'emms-show)
     (global-set-key (kbd "C-c e >") 'emms-seek-forward)
     (global-set-key (kbd "C-c e <") 'emms-seek-backward)
     ;; these keys maps were derivations of above keybindings
     (global-set-key (kbd "C-c e S") 'emms-start)
     (global-set-key (kbd "C-c e g") 'emms-playlist-mode-go)
     (global-set-key (kbd "C-c e t") 'emms-play-directory-tree)
     (global-set-key (kbd "C-c e h") 'emms-shuffle)
     (global-set-key (kbd "C-c e e") 'emms-play-file)
     (global-set-key (kbd "C-c e l") 'emms-play-playlist)
     (global-set-key (kbd "C-c e r") 'emms-toggle-repeat-track)
     (global-set-key (kbd "C-c e R") 'emms-toggle-repeat-playlist)
     (global-set-key (kbd "C-c e u") 'emms-score-up-playing)
     (global-set-key (kbd "C-c e d") 'emms-score-down-playing)
     (global-set-key (kbd "C-c e o") 'emms-score-show-playing)

     ;; coding settings
     (setq emms-info-mp3info-coding-system 'gbk
           emms-cache-file-coding-system 'utf-8
           ;; emms-i18n-default-coding-system '(utf-8 . utf-8)
           )
     ))
