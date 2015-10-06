(require 'editorconfig-fnmatch)

(require 'editorconfig-core-handle)


(defun editorconfig-core--merge-properties (current &rest rest)
  "Merge CURRENT and REST, which are alists of properties.

Latter property alists take precedence. For examle, when called like

(editorconfig-core--merge-properties '((a . 1) (c . 1))
                                     '((a . 2) (b . 2)))

then the result will be

'(
  (a . 2) (b . 2) (c . 1)
  ) ."
  (if rest
      (editorconfig-core--merge-properties (editorconfig-core--merge-two-properties current (car rest))
                                           (cdr rest))
    current))

(defun editorconfig-core--merge-two-properties (old new)
  "Merge two property alist.
Properties in OLD will be overwritten by properties in NEW.
This function is non-destructive."
  (if old
      (let ((result (copy-alist old)))
        (dolist (e new)
          (let ((pair (assoc (car e)
                             result)))
            (if pair
                (setcdr pair
                        (cdr e))
              (setq result
                    `(,@result ,e)))))
        result)

    ;; If OLD is nil return NEW as it is
    (copy-alist new)))

(defun editorconfig-core-get-properties-from (file conf)
  "Get EditorConfig properties for FILE from CONF.")

;;;###autoload
(defun editorconfig-core-get-properties (file &optional confname version)
  "Get EditorConfig properties for FILE.

Pass arg CONFNAME to use config file other than \".editorconfig\"."
  (setq file (expand-file-name file))
  (setq confname (or confname
                     ".editorconfig"))
  '("key" . "value"))

(provide 'editorconfig-core)
