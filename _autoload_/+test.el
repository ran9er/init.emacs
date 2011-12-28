;; -*- encoding: utf-8-unix; -*-
;; File-name:    <test.el>
;; Create:       <2011-12-25 20:23:08 ran9er>
;; Time-stamp:   <2011-12-27 21:34:32 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun mklst (n)
  "创建大小为 n 的字符串列表"
  (let* ((i n)(x nil))
    (while (> i 0)
      (setq x (cons (number-to-string i) x))
      (setq i (1- i)))
    x))

;;;###autoload
(defun eval-buffer-time ()
  ""
  (interactive)
  (let ((tm (float-time)))
    (eval-buffer)
    (message (number-to-string (- (float-time) tm)))))

;;;###autoload
(defmacro test-list (n &rest fn)
  "用大小为 n 的字符串列表，测试函数 fn (fn 最后一个参数为列表)"
  `(,@fn (mklst ,n)))

;;;###autoload
(defmacro test-times (n &rest body)
  "计算 body 运行 n 次所需时间"
  `(let ((tm ,n)(beg (float-time)))
     (while (> tm 0)
       (progn ,@body)
       (setq tm (1- tm)))
     (- (float-time) beg)
     ))

;(test-times 100 (test-list 9 define-key-s (current-local-map)))
