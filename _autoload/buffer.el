;; -*- encoding: utf-8-unix; -*-
;; File-name:    <bu.el>
;; Create:       <2011-12-27 16:45:19 ran9er>
;; Time-stamp:   <2011-12-28 10:12:18 ran9er>
;; Mail:         <2999am@gmail.com>

;;;###autoload
(defun substring-buffer-name (m n &optional x)
  "使用 substring 截取文件名时，在 buffer-name 后面加几个字符，\
防止文件名过短引发错误。m n 参数同`substring'的 from to，可选参数\
 x 存在时截取带路径的文件名。"
  (substring (concat
              (if x
                  (buffer-file-name)
                (buffer-name))
              (make-string n ?*))
             m n))

