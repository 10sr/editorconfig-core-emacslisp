(require 'editorconfig-fnmatch)

(require 'cl-lib)
(require 'subr-x)


(defvar editorconfig-core-handle--cache-hash
  (make-hash-table :test 'equal)
  "Hash of EditorConfig filename and its handle instance.

Value should be a list like (MTIME TOP-PROP PROP), where

MTIME: The mtime of config file
TOP-PROP: Alist of (KEY . VALUE) pair of top properties (properties which does
  not associated with any patterns)
PROP: Alist of (PATTERN . PROPS), where PROPS is alist of (KEY . VALUE) pair
PATH: Path to config file.")

(cl-defstruct editorconfig-core-handle
  (top-prop nil)
  (prop nil)
  (mtime nil)
  (path nil))



(defun editorconfig-core-handle-get (conf)
  "Return EditorConfig handle for CONF, which should be a file path.

If CONF does not exist return nil."
  (when (file-readable-p conf)
    (let ((cached (gethash conf
                           editorconfig-core-handle--cache-hash))
          (mtime (nth 5
                      (file-attributes conf))))
      (if (and cached
               (= (editorconfig-core-handle-mtime cached)
                  mtime))
          cached
        (let ((parsed (editorconfig-core-handle--parse-file conf)))
          (puthash conf
                   (make-editorconfig-core-handle :top-prop (car parsed)
                                                  :prop (cdr parsed)
                                                  :mtime mtime
                                                  :path conf)
                   editorconfig-core-handle--cache-hash))))))


(defun editorconfig-core-handle-root-p (handle)
  "Return non-nil if HANDLE represent root EditorConfig file."
  (string= "true"
           (cdr (assoc "root"
                       (editorconfig-core-handle-top-prop handle)))))

(defun editorconfig-core-handle-get-properties (handle file)
  "Return list of alist of properties for FILE from HANDLE."
  (mapcar 'cdr
          (cl-remove-if-not (lambda (prop)
                              (editorconfig-fnmatch-p file
                                                      (car prop)))
                            (editorconfig-core-handle-prop handle))))

(defun editorconfig-core-handle--parse-file (conf)
  "Parse EditorConfig file CONF and return cons of its top properties alist and
alist of patterns and its properties alist.

If CONF is not found return nil."
  (when (file-readable-p conf)
    (with-temp-buffer
      (insert-file-contents-literally conf)
      (goto-char (point-min))
      (let ((point-max (point-max))
            (all-props ())
            (top-props nil)

            ;; String of current line
            (line "")
            ;; nil when pattern not appeared yet, "" when pattern is empty ("[]")
            (pattern nil)
            ;; Alist of properties for current PATTERN
            (props ())

            )
        (while (not (eq (point) point-max))
          (setq line
                (buffer-substring-no-properties (point-at-bol)
                                                (point-at-eol)))
          (setq line
                (string-trim (replace-regexp-in-string "\\(^\\|[^//]\\| \\)\\(#\\|;\\).*$"
                                                       ""
                                                       line)))
          (cond
           ((string-empty-p line)
            nil)

           ((string-match "^\\[\\(.*\\)\\]$"
                          line)
            (when pattern
              (setq all-props
                    `(,@all-props (,pattern . ,props)))
              (setq props nil))
            (setq pattern (match-string 1 line)))

           ((string-match-p "=\\|:"
                            line)
            ;; (or (string-match "^\\([^=]*\\)=\\(.*\\)$"
            ;;                   line)
            ;;     (string-match "^\\([^:]*\\):\\(.*\\)$"
            ;;                   line))
            ;; NOTE: Using match-string does not work as expected
            (let* (
                   ;; (key (downcase (string-trim (match-string 1 line))))
                   ;; (value (downcase (string-trim (match-string 2 line))))
                   (idx (string-match "=\\|:"
                                      line))
                   (key (downcase (string-trim (substring line
                                                          0
                                                          idx))))
                   (value (downcase (string-trim (substring line
                                                            (1+ idx)))))
                  )
              (if pattern
                  (setq props
                        `(,@props (,key . ,value)))
                (setq top-props
                      `(,@top-props (,key . ,value))))))
           )
          (forward-line 1))
        (when pattern
          (setq all-props
                `(,@all-props (,pattern . ,props))))
        (cons top-props
              all-props)))))

;;(editorconfig-core-handle--parse-file "./tests-ert/fixtures/handle.ini")
;;(setq debug-on-error t)


(provide 'editorconfig-core-handle)
