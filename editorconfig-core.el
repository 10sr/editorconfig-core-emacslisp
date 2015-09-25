(defun editorconfig-core-get-properties (file &optional confname version)
  "Get EditorConfig properties for FILE.

Pass arg CONFNAME to use config file other than \".editorconfig\"."
  ;; Return hash
  '("key" . "value"))

(provide 'editorconfig-core)
