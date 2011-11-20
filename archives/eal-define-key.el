;;;###autoload
(defun eal-define-keys (keymaps key-defs)
  "Execute `define-key' on KEYMAPS by `eval-after-load' technique use arguments from element of list KEY-DEFS.

KEY-DEFS should be one list, every element of it is a list
whose first element is key like argument of `define-key', and second element is command
like argument of `define-key'."
  (eal-eval-by-maps
   keymaps
   `(lambda (keymap)
      (eal-define-keys-commonly (symbol-value keymap) ',key-defs))))

;;;###autoload
(defun eal-define-keys-commonly (keymap key-defs)
  "Execute `define-key' on KEYMAP use arguments from KEY-DEFS.

KEY-DEFS should be one list, every element of it is a list
whose first element is key like argument of `define-key', and second element is command
like argument of `define-key'."
   (dolist (key-def key-defs)
     (when key-def
       (define-key keymap (eval `(kbd ,(car key-def))) (nth 1 key-def)))))

;;;###autoload
(defun eal-define-key (keymap key def)
  "Execute `define-key' use arguments KEYMAP, KEY, DEF by `eval-after-load' technique.

*Note*: KEYMAP should be quoted, this is diference between argument of `define-key'."
  (eal-eval-by-maps
   keymap
   `(lambda (keymap)
      (define-key (symbol-value keymap) ,key ',def))))
