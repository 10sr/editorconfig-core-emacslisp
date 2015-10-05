(require 'editorconfig-fnmatch)

(require 'editorconfig-core-handle)


(defun editorconfig-core--merge-properties (list)
  "Merge LIST, which has property alist as its element.

The first property alist takes precedence. For examle, when LIST is

'(
  ((a . 2) (c . 2))
  ((a . 1) (b . 1))
)

then the result will be

'(
  ((a . 2) (b . 1) (c . 2))
)."
  (if (cdr list)
      (editorconfig-core--merge-two-properties (car list)
                                               (editorconfig-core--merge-properties (cdr list)))
    (car list)))

(defun editorconfig-core--merge-two-properties (new old)
  "Merge two property alist.")

(defun editorconfig-core-get-properties-from (file conf)
  "Get EditorConfig properties for FILE from CONF.")

(defun editorconfig-core-get-properties (file &optional confname version)
  "Get EditorConfig properties for FILE.

Pass arg CONFNAME to use config file other than \".editorconfig\"."
  (setq file (expand-file-name file))
  (setq confname (or confname
                     ".editorconfig"))
  '("key" . "value"))

(provide 'editorconfig-core)
