(defun editorconfig-core-wildcard-match-p (target pattern &optional dir)
  "Test if PATTERN  matches with TERGET filename.

Optional third argument DIR specifies the base directory for PATTERN, which
defaults to `default-directory'."
  ;; FIXME: This is POOR implementation. Should parse PATTERN first!
  (let ((pattern-regexp (replace-regexp-in-string
                         "\\(\\.\\|\\*\\*\\|\\*\\|\\?\\|\\[[^\]]*\\]\\|{[^}]*}\\)"
                         (lambda (matched)
                           (cond
                            ((string= "."
                                      matched)
                             "\\\\.")
                            ((string= "**"
                                      matched)
                             ".*")
                            ((string= "*"
                                      matched)
                             "[^\/]*")
                            ((string= "?"
                                      matched)
                             ".")
                            ;; ((string-match-p "\\[^[^\]]*\\]"
                            ;;                  matched)
                            ;;  (let ((chars (mapcar 'identity
                            ;;                       (substring matched
                            ;;                                  1
                            ;;                                  -1))))
                            ;;    (concat "\\\\("
                            ;;            (mapconcat 'string
                            ;;                       chars
                            ;;                       "\\\\|")
                            ;;            "\\\\)")))
                            ;; ((string-match-p "\\[[^\]]*\\]"
                            ;;                  matched)
                            ;;  (let ((chars (mapcar 'identity
                            ;;                       (substring matched
                            ;;                                  1
                            ;;                                  -1))))
                            ;;    (concat "\\\\("
                            ;;            (mapconcat 'string
                            ;;                       chars
                            ;;                       "\\\\|")
                            ;;            "\\\\)")))
                            ((string-match-p "\\[[^\]]*\\]"
                                             matched)
                             matched)
                            ((string-match-p "{[^}]*}"
                                             matched)
                             (let ((c (split-string (substring matched
                                                               1
                                                               -1)
                                                    ",")))
                               (concat "\\\\("
                                       (mapconcat 'identity
                                                  c
                                                  "\\\\|")
                                       "\\\\)"))
                             )
                            ))
                         pattern)))
    ;; (string-match-p pattern-regexp
    ;;                 target)
    pattern-regexp
    ))


(replace-regexp-in-string
 "{[^}]*}"
 (lambda (matched)
   (let ((c (split-string (substring (copy-sequence matched)
                                     1
                                     -1)
                          ",")))
     (let ((replace (concat "\\\\("
                            (mapconcat 'identity
                                       c
                                       "\\\\|")
                            "\\\\)")))
       replace)))
 "{py,js}")
(string-match-p "{[^}]*}" "{abc}")
(editorconfig-core-wildcard-match-p "js" "{py,js}")

(defun editorconfig-core-get-properties (file &optional confname version)
  "Get EditorConfig properties for FILE.

Pass arg CONFNAME to use config file other than \".editorconfig\"."
  ;; Return hash
  '("key" . "value"))

(provide 'editorconfig-core)
