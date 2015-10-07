(require 'editorconfig-fnmatch)

(require 'editorconfig-core-handle)


;;; Code:

(defconst editorconfig-core-version
  "0.0.1"
  "Current EditorConfig core version.")

(defun editorconfig-core--merge-properties (&optional current &rest rest)
  "Merge CURRENT and REST, which are alists of properties.

Latter property alists take precedence. For examle, when called like

(editorconfig-core--merge-properties \'((a . 1) (c . 1))
                                     \'((a . 2) (b . 2)))

then the result will be

\'(
   (a . 2) (c . 1) (b . 2)
   ) ."
  (if rest
      (apply 'editorconfig-core--merge-properties (editorconfig-core--merge-two-properties current (car rest))
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

(defun editorconfig-core-get-handles (dir confname &optional result)
  "Get list of EditorConfig handlers for DIR from CONFNAME."
  (setq dir (expand-file-name dir))
  (let ((handle (editorconfig-core-handle (concat (file-name-as-directory dir)
                                                  confname)))
        (parent (file-name-directory (directory-file-name dir))))
    (if (or (string= parent
                     dir)
            (and handle
                 (editorconfig-core-handle-root-p handle)))
        (cons handle result)
      (editorconfig-core-get-handles parent
                                     confname
                                     (cons handle
                                           result)))))

;;;###autoload
(defun editorconfig-core-get-properties (file &optional confname version)
  "Get EditorConfig properties for FILE.

Pass arg CONFNAME to use config file other than \".editorconfig\"."
  (setq file (expand-file-name file))
  (setq confname (or confname
                     ".editorconfig"))
  (let ((result (apply 'editorconfig-core--merge-properties
                       (mapcar (lambda (arg)
                                 (apply 'editorconfig-core--merge-properties
                                        arg))
                               (mapcar (lambda (handle)
                                         (editorconfig-core-handle-get-properties handle
                                                                                  file))
                                       (editorconfig-core-get-handles (file-name-directory file)
                                                                      confname))))))
    (dolist (key '("end_of_line" "indent_style" "indent_size"
                   "insert_final_newline" "trim_trailing_whitespace" "charset"))
      (let ((pair (assoc key
                         result)))
        (when pair
          (setcdr pair
                  (downcase (cdr pair))))))

    (let ((indent-size (assoc "indent_size" result))
          (tab-width (assoc "tab_width" result)))
      ;; Add indent_size property
      (when (and (not indent-size)
                 (string= (cdr (assoc "indent_style" result)) "tab"))
        ;; TODO: Add version condition
        (setq result
              `(,@result ("indent_size" . "tab")))))
    (let ((indent-size (assoc "indent_size" result))
          (tab-width (assoc "tab_width" result)))
      ;; Add tab_width property
      (when (and indent-size
                 (not tab-width)
                 (not (string= (cdr indent-size) "tab")))
        (setq result
              `(,@result ("tab_width" . ,(cdr indent-size))))))
    (let ((indent-size (assoc "indent_size" result))
          (tab-width (assoc "tab_width" result)))
      ;; Update indent-size property
      (when (and indent-size
                 tab-width
                 (string= (cdr indent-size) "tab"))
        (setcdr indent-size (cdr tab-width))))

    result))

(provide 'editorconfig-core)
