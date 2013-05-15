;;; desktop-frame.el --- desktop + frame save-restore.

;;; Copyright (C) 1998 Chuck Siska

;;; Author: Chuck Siska <chucks@ics.uci.edu>
;;; Maintainer: chucks@ics.uci.edu
;;; Keywords: extensions
;;; Created: 1998

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Quick Start:
;;
;;   To save desktop & frame-layout:  M-x desktop-frame-save
;;
;;   To restore: C-x 1, M-x desktop-read, M-x load-file "<dir>/emacs.frx"
;;
;;   Warning: non-"desktop-read" (e.g., *shell*) buffers' windows will
;;      display the "C-x 1" buffer.
;;
;; This is a primitive no-frills attempt at handling frame-layouts.  We
;; ask for a directory in which to save both the desktop and the
;; frame-layout.  The default frame-layout file name is "emacs.frx"
;; (ms-dos & windows-nt) or "emacs.framex" depending on your system
;; type.  This is similar to that for the default desktop filename.
;; This default name is customized by changing the frame-layout-
;; basefilename defconst in this code file.
;;
;; M-x desktop-frame-save saves the desktop to the default desktop file
;; prior to saving the frame-layout file so that we don't have to
;; duplicate the desktop-save work.  You'll want to restore the desktop
;; (just do desktop-read with the save-directory as an argument) before
;; doing the load-file.
;;
;; Before doing the load-file to restore the frame-layout, reduce the
;; current frame-layout to a single window.  We currently only restore
;; frame-layouts after a reboot, so having a single window is natural.
;; The restoration should not be expected to work if the frame has more
;; than one window.  If it doesn't, you can always try it again.
;;
;; When restoring, we try to display the buffer, by name, in each window
;; as it was displayed on saving.  If the buffer has a different name,
;; you'll see the "single window from which the load-file was performed"
;; in its place.  
;;
;; We handle a reasonably complex frame-layout of windows.  Example of
;; complexity handled:
;;
;;                                it's window tree showing
;;    a frame-layout              horizontal & vertical splits
;;
;;    +-------+-----+--------+     0----4--6
;;    |       |     |        |     |    |  |
;;    |   0   |     |   6    |     1-2  5  7-8
;;    |       |  4  |        |       |     |
;;    +---+---+     +---+----+       3     9
;;    |   |   +-----+   |    |
;;    |   | 2 |     | 7 |  8 |
;;    |   |   |     +---+----+
;;    |   |   |     |        |
;;    | 1 +---+  5  |        |
;;    |   |   |     |   9    |
;;    |   | 3 |     |        |
;;    +---+---+-----+--------+
;;
;; We create a buffer, *frame-layout*, which is saved to the frame-
;;  layout file.
;; Restore with only one window open -- we don't kill other windows.
;; We don't restore frame font -- do this first before restoration.
;; We don't restore the desktop -- do this first before restoration.
;; We don't restore non-desktop-read windows -- these remain whatever
;;  buffer was in the window in which you did the restoration.
;; See the TO DO list for the rest of what isn't done.
;; Internal functions are prefixed "csdf-".
;; Fixed "window too small" error on vertical split.

;; TO DO:
;;
;; Handle more complex trees, CF TBD in csdf-window-splits-cmds.
;; Restore other non-desktop-read kinds of buffers.
;;  (Quick cut: first, save to file just their displayed lines)
;; Create a desktop-frame-read coordinated with desktop-read (which
;;  apparently shouldn't be called directly).
;; Pretty-print the frame-layout commands in *frame-layout*, and hence
;;  in the frame-layout file.
;;
;; Integrate into desktop.el.  Note, there may be some value in keeping
;; the desktop buffer restoration and the frame-layout restoration
;; separate.  Not everyone may be interested in getting a complete
;; makeover of the frame-layout when they just want to include the
;; buffers from some other desktop-save.


;;; Code:

;; We use the desktop-save function from this module
(require 'desktop)

;;******************************************************* For Users ****
(defconst frame-layout-basefilename
  (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
      "emacs.frx" ; Ms-Dos does not support multiple dots in file name
    ".emacs.framex")
  "File for Emacs frame-layout, not including the directory name.")

;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ End of For Users ^^^^

;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv csdf-dbg-window-splits vvvv
(defvar csdf-dbg-window-splits nil
  "Internal debugging variable.")

;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv csdf-window-00 vvvv
(defun csdf-window-00 ()
  "Return the 0,0 coordinate window in the frame's cycle of windows."
  (let ((w00 nil))
    (walk-windows
     (function (lambda (wx)
                 (let ((wx-edges (window-edges wx)))
                   (if (and (= 0 (car wx-edges))
                            (= 0 (nth 1 wx-edges)))
                       (setq w00 wx))))))
    w00))

;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv csdf-window-triples vvvv
(defun csdf-window-triples ()
  "Return list of window triples (<cycle-order> <edges> <window>)
in cycle-order for each window, excluding minibuffer.  Example:
    ((0 (0 0 68 59) #<window 3 on _emacs>)
     (1 (0 59 33 119) #<window 13 on *shell*>)
     ...)"
  (let ((cid 0)
        (w00 (csdf-window-00))
        (lx nil))
    (save-selected-window
      (select-window w00)
      (select-window (previous-window))
      (walk-windows
       (function
        (lambda (wx)
          (if (not (window-minibuffer-p wx))
              (progn
                (setq lx (append lx (list (list cid
                                                (window-edges wx)
                                                wx))))
                (setq cid (+ 1 cid))))))
       nil))
    lx))

;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv csdf-window-triples-sorted vvvv
(defun csdf-window-triples-sorted ()
  "Return list of window triples  (<cycle-order> <edges> <window>)
sorted by left-edge first and top-edge second."
  (let ((lx (csdf-window-triples)))
      (sort lx
            (function (lambda (aw bw)
                        (let ((ax (nth 1 aw))
                              (bx (nth 1 bw)))
                          (cond ((< (car ax) (car bx))
                                 t)
                                ((= (car ax) (car bx))
                                 (< (nth 1 ax) (nth 1 bx)))
                                (t nil))))))
      lx))

;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv csdf-match-0th-or-1st vvvv
(defun csdf-match-0th-or-1st (le1 le2)
  "Return 0, 1 or nil match given two numeric lists.
0 for match in 0th list position, 1 for match in 1st list position,
else nil."
  (and (listp le1)
       (listp le2)
       (if (eq (car le1) (car le2))
           0
         (if (eq (nth 1 le1) (nth 1 le2))
             1
           nil))))

;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv csdf-window-splits vvvv
(defun csdf-window-splits ()
  "Return list of quadruples (<cycle-order> <mom> <split> <edge-win>)
in cycle-order where mom is window's parent in the frame's window tree,
split is 0 for -vertical, 1 for -horizontal, or nil for root, and
edge-win is a sublist of the window's edges and the window, itself.
Example:
    ((0 -1 nil ((0 0 68 59) #<window 3 on _emacs>))
     (1 0 0 ((0 59 33 119) #<window 13 on *shell*>))
     (2 1 1 ((33 59 67 89) #<window 19 on fred.el>))
     ...)
Also, places the result in debug var, csdf-dbg-window-splits."
  (let ((lx (csdf-window-triples-sorted))
        (ix 0)
        (jx 1)
        (iy 0)
        (lx-len 0)
        (i-edges nil)
        (j-edges nil)
        (matchx nil)
        (sx nil)) ; result quadruples, in reverse order.
    (setq lx-len (length lx)
          ; init 0 as hierarchy root.
          sx `((0 -1 nil ,(cdr (car lx)))))
    (while (< ix lx-len)
      (setq iy ix)
      (setq jx (+ 1 ix))
      (setq j-edges (nth 1 (assoc jx lx)))
      (while (< -1 iy)
        (setq i-edges (nth 1 (assoc iy lx)))
        (setq matchx  (csdf-match-0th-or-1st i-edges j-edges))
        ; matchx is 0 for for -vertical, 1 for -horizontal, or nil.
        (if (not (null matchx))
            (progn
              (setq sx (append (list (list jx iy matchx
                                           (cdr (assoc jx lx))))
                               sx))
              (setq iy -2))
          ; else null matchx.
          (setq iy (nth 1 (assoc iy sx))))) ; get parent of iy.
      (setq ix (+ 1 ix)))
    (setq csdf-dbg-window-splits (reverse sx)) ;DEBUG, for poking around.
    (reverse sx)))

;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv csdf-window-splits-ready vvvv
(defun csdf-window-splits-ready ()
  "Return list of quadruples (<mom> <cycle-order> <split> <edge-win>),
see csdf-window-splits, in order of mom first and reverse cycle-order
second.  This puts the windows in a simple order from which to
create a sequence of window-split commands.  Note, the first element
has mom -1 which indicates the root window."
  (let ((lx (csdf-window-splits)))
      (sort
       (mapcar (function (lambda (elt)
                           (append (list (nth 1 elt)) (list (car elt))
                                   (cdr (cdr elt)))))
               lx)
       (function (lambda (ax bx)
                   (or
                    (< (car ax) (car bx))
                    (and (= (car ax) (car bx))
                         (> (nth 1 ax) (nth 1 bx)))))))))

;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv csdf-window-splits-cmds vvvv
(defun csdf-window-splits-cmds ()
  "Return self-contained list of window split commands to regenerate
current frame split layout.  Note, the exact geometry also involves
restoration of 1) the frame size, 2) the frame font, and once these
are done, 3) the frame window sizes.  Also, see TBD in function body.
Example:
    (save-selected-window
      (split-window-horizontally)
      (split-window-vertically)
      (select-window (next-window))
      (split-window-horizontally)
      ...)"
  (let ((sx2 (cdr (csdf-window-splits-ready))) ;cdr -> skip root window.
         (elt nil)
         (elt-1 nil) ; first elt.
         (elt-prev nil) ; previous elt.
         (elt-wid nil) ; current elt's window id.
         (elt-wid-prev nil) ; previous elt's window id.
         diffx ; curr elt's wid minus prev elt's wid.
         splitx ; which way to split.
         (cmds '(save-selected-window))) ; list of commands to execute.
    (setq elt (car sx2)) ; seed current elt from split list.
    (setq elt-1 elt) ; remember first elt.
    (while (not (null elt))
      (setq diffx 0 ; default.
            elt-wid-prev -1 ; default.
            splitx (nth 2 elt) ; which way to split for curr elt.
            elt-wid (car elt)) ; wid of current elt.
      ; setup diffx & elt-wid-prev.
      (if (not (null elt-prev))
          (progn (setq elt-wid-prev (car elt-prev))
                 (setq diffx (- elt-wid elt-wid-prev))))
      ; if changed wid-s, move into newly split off window.
      (if (< 0 diffx)
          (setq cmds (append cmds '((select-window (next-window))))))
      ; if wid diff is more than 1, move into ancestor's horz-split off
      ; window.  TBD, this is close enough for my taste in frame layout
      ; complexity, but is not strictly correct for really deeply
      ; nested windows.  What's needed is to "backup" to the correct
      ; ancestor window in the window tree.
      (if (< 1 diffx)
          (setq cmds (append cmds '((select-window (next-window))))))
      (cond
       ((null splitx) nil)
       ((= 0 splitx)
        ; if split off vertically.
        (if (eq elt elt-1)
            ; first window -- don't enlarge it.
            (setq cmds (append cmds `((split-window-vertically))))
          (setq cmds (append cmds `((if (> 6 (window-height))
                                        (enlarge-window
                                         (- 8 (window-height))))
                                    (split-window-vertically)))) ))
       ; if split off horizontally.
       ((= 1 splitx)
        (if (eq elt elt-1)
            ; first window -- don't enlarge it
            (setq cmds (append cmds `((split-window-horizontally))))
          (setq cmds (append cmds `((if (> 6 (window-width))
                                        (enlarge-window
                                         (- 8 (window-width)) t))
                                    (split-window-horizontally)))) )))
      (setq elt-prev elt ; cache new previous elt.
            sx2 (cdr sx2)) ; remove current elt from split list.
      (setq elt (car sx2))) ; setup new current elt.
    cmds))

;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv csdf-window-buffer-cmds vvvv
(defun csdf-window-buffer-cmds ()
  "Return self-contained list of buffer-in-window display commands.
Only involves buffers restored by desktop-read, by name -- other
buffers are ignored.  To be executed after the csdf-window-splits-cmds
commands (so that the correct window layout has been restored)."
  (let ((lx (csdf-window-splits))
        (elt nil)
        (edgwin nil)
        (win nil)
        (width nil)
        (height nil)
        (buf nil)
        (bufname nil)
        (cmds2 '(save-selected-window)))
    (setq elt (car lx))
    (while (not (null elt))
      ; example elt, (1 0 0 ((0 59 34 119) #<window 13 on *shell*>)).
      (setq edgwin (nth 3 elt))
      (setq win (nth 1 edgwin))
      (setq width (window-width win)
            height (window-height win))
      (setq cmds2 (append cmds2
                          `(; cols.
                            (enlarge-window (- ,width (window-width)) t)
                            ; rows.
                            (enlarge-window (- ,height
                                               (window-height))))))
      (if (window-live-p win)
          (progn
            (setq buf (window-buffer win))
            (setq bufname (buffer-name buf))
            (setq cmds2
                  (append cmds2
                          `((if (get-buffer ,bufname)
                                (progn
                                  (set-window-buffer
                                   (selected-window)
                                   ,bufname)
                                  (set-window-point
                                   (selected-window)
                                   ,(window-point win))
                                  (set-window-start
                                   (selected-window)
                                   ,(window-start win)
                                   nil))))))))
      (setq cmds2 (append cmds2
                          `((select-window (next-window)))))
      (setq lx (cdr lx))
      (setq elt (car lx)))
    cmds2))

;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv csdf-frame-layout-cmds vvvv
(defun csdf-frame-layout-cmds ()
  "Return frame layout restoration commands."
  (let ((cmds `(progn
                 (set-frame-size (selected-frame)
                                 ,(frame-width)
                                 ,(frame-height)))))
    (append cmds
            (list (csdf-window-splits-cmds))
            (list (csdf-window-buffer-cmds)))))

;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv desktop-frame-save vvvv
(defun desktop-frame-save (dirname)
  "Save the Desktop & Frame-layout file in the given, DIRNAME, directory.
To restore a Frame-layout, ensure C-x 1, do a desktop-read
and then a load-file of the frame-layout file."
  (interactive "DDirectory in which to save the Frame-layout file: ")
;;  (desktop-save dirname)
  (save-excursion
    (let ((filename
           (expand-file-name (concat dirname frame-layout-basefilename)))
	  (buf (get-buffer-create "*frame-layout*"))
	  (cmds (csdf-frame-layout-cmds)))
      (set-buffer buf)
      (erase-buffer)
      (insert ";;; -*- Mode: Lisp -*-\n;;;\n"
              ";;; Frame Layout File for Emacs\n"
              ";;;----------------------------\n"
	      ";; Created " (current-time-string) "\n"
	      ";; Emacs version " emacs-version "\n\n"
	      ";; Frame-layout section:\n")
      (print cmds buf)
      (setq default-directory dirname) ; set buffer's local dir var.
      (if (file-exists-p filename)
          (delete-file filename))
      (write-region (point-min) (point-max) filename nil 'nomessage)
      (message "Frame-layout saved to *** %s ***" filename))))

;;========================================================= provide ====
(provide 'desktop-frame)

;; end of desktop-frame.el
