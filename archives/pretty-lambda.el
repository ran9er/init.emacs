;; -*- encoding: utf-8-unix; -*-
;; File-name:    <pretty-lambda.el>
;; Create:       <2011-11-20 10:17:27 ran9er>
;; Time-stamp:   <2011-11-20 10:17:32 ran9er>
;; Mail:         <2999am@gmail.com>
;; * pretty lambda
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

