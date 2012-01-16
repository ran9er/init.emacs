;; -*- encoding: utf-8-unix; -*-
;; File-name:    <04_advice.el>
;; Create:       <2012-01-16 13:44:23 ran9er>
;; Time-stamp:   <2012-01-16 17:04:03 ran9er>
;; Mail:         <2999am@gmail.com>

(defadvice comment-or-uncomment-region (before slickcomment activate compile)
  "When called interactively with no active region, toggle comment on current line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))


;; (defadvice kill-region (before smart-kill (beg end) activate)
;;   (let ((p (point))
;;         (i (save-excursion (abs (skip-chars-backward " \t")))))
;;     (cond
;;      ((if mark-active
;;           (setq beg (mark)
;;                 end p)))
;;      ((< 0 i)
;;        (if (zerop (mod i tab-width))
;;            (setq beg (- p tab-width)
;;                  end p)
;;          (setq beg (- p (mod i tab-width))
;;                end p)))
;;      (t
;;       (progn (backward-word)(setq beg (point)
;;                                   end p))))))

