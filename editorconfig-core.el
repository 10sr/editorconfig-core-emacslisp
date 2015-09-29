(defun editorconfig-core-wildcard-match-p (target pattern &optional dir)
  "Test if PATTERN  matches with TERGET filename.

Optional third argument DIR specifies the base directory for PATTERN, which
defaults to `default-directory'."
  t)

(defun editorconfig-core-get-properties (file &optional confname version)
  "Get EditorConfig properties for FILE.

Pass arg CONFNAME to use config file other than \".editorconfig\"."
  ;; Return hash
  '("key" . "value"))

(provide 'editorconfig-core)
