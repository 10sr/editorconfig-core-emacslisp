;;; editorconfig-core.el --- EditorConfig Core library written purely in Emacs Lisp

;; Author: 10sr <8slashes+el [at] gmail [dot] com>
;; URL: https://github.com/10sr/editorconfig-core-emacslisp
;; Version: 0.1.4
;; Keywords: utility editorconfig
;; Package-Requires: ((editorconfig-fnmatch "20151023.1021") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This library is one implementation of EditorConfig Core, which parses
;; .editorconfig files and returns properties for given files.
;; This can be used in place of, for example, editorconfig-core-c.

;; This library is not an editor plugin and does not configure Emacs for editing
;; the files: this should be done with editorconfig-emacs.


;; Functions

;; editorconfig-core-get-properties (&optional file confname confversion)

;; Get EditorConfig properties for FILE.

;; If FILE is not given, use currently visiting file.
;; Give CONFNAME for basename of config file other than .editorconfig.
;; If need to specify config format version, give CONFVERSION.

;; This functions returns alist of properties. Each element will look like
;; (KEY . VALUE) .


;; editorconfig-core-get-properties-hash (&optional file confname confversion)

;; Get EditorConfig properties for FILE.

;; This function is almost same as `editorconfig-core-get-properties', but
;; returns hash object instead.

;;; Code:

(require 'editorconfig-core-handle)


(defconst editorconfig-core-version
  "0.1.4"
  "EditorConfig core version.")

(defun editorconfig-core--remove-duplicate (alist)
  "Remove duplicated keys in ALIST.

If same keys are found in ALIST multiple times, the latter ones take precedence.
For example, when ALIST is

    '((a 1) (b 2) (c 3) (b 4))

then the result will be

    '((a 1) (b 4) (c 3)) ."
  (let ((result ()))
    (dolist (e alist)
      (let ((pair (assoc (car e)
                         result)))
        (if pair
            (setcdr pair
                    (cdr e))
          (setq result
                `(,@result ,e)))))
    result))

(defun editorconfig-core--get-handles (dir confname &optional result)
  "Get list of EditorConfig handlers for DIR from CONFNAME.

RESULT is used internally and normally should not be used."
  (setq dir (expand-file-name dir))
  (let ((handle (editorconfig-core-handle (concat (file-name-as-directory dir)
                                                  confname)))
        (parent (file-name-directory (directory-file-name dir))))
    (if (or (string= parent
                     dir)
            (and handle
                 (editorconfig-core-handle-root-p handle)))
        (cons handle result)
      (editorconfig-core--get-handles parent
                                     confname
                                     (cons handle
                                           result)))))


;;;###autoload
(defun editorconfig-core-get-properties (&optional file confname confversion)
  "Get EditorConfig properties for FILE.
If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.
If need to specify config format version, give CONFVERSION.

This functions returns alist of properties.  Each element will look like
'(KEY . VALUE) ."
  (setq file (expand-file-name (or file
                                   buffer-file-name
                                   (error "FILE is not given and `buffer-file-name' is nil"))))
  (setq confname (or confname
                     ".editorconfig"))
  (setq confversion (or confversion
                        "0.12.0"))
  (let ((result (editorconfig-core--remove-duplicate
                 (apply 'append
                        (mapcar (lambda (handle)
                                  (apply 'append
                                         (editorconfig-core-handle-get-properties handle
                                                                                  file)))
                                (editorconfig-core--get-handles (file-name-directory file)
                                                               confname))))))
    (dolist (key '("end_of_line" "indent_style" "indent_size"
                   "insert_final_newline" "trim_trailing_whitespace" "charset"))
      (let ((pair (assoc key
                         result)))
        (when pair
          (setcdr pair
                  (downcase (cdr pair))))))

    ;; Add indent_size property
    (let ((indent-size (assoc "indent_size" result))
          (tab-width (assoc "tab_width" result)))
      (when (and (not indent-size)
                 (string= (cdr (assoc "indent_style" result)) "tab")
                 ;; If VERSION < 0.9.0, indent_size should have no default value
                 (version<= "0.9.0"
                            confversion))
        (setq result
              `(,@result ("indent_size" . "tab")))))
    ;; Add tab_width property
    (let ((indent-size (assoc "indent_size" result))
          (tab-width (assoc "tab_width" result)))
      (when (and indent-size
                 (not tab-width)
                 (not (string= (cdr indent-size) "tab")))
        (setq result
              `(,@result ("tab_width" . ,(cdr indent-size))))))
    ;; Update indent-size property
    (let ((indent-size (assoc "indent_size" result))
          (tab-width (assoc "tab_width" result)))
      (when (and indent-size
                 tab-width
                 (string= (cdr indent-size) "tab"))
        (setcdr indent-size (cdr tab-width))))

    result))

;;;###autoload
(defun editorconfig-core-get-properties-hash (&optional file confname confversion)
  "Get EditorConfig properties for FILE.
If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.
If need to specify config format version, give CONFVERSION.

This function is almost same as `editorconfig-core-get-properties', but returns
hash object instead."
  (let ((result (editorconfig-core-get-properties file
                                                  confname
                                                  confversion))
        (hash (make-hash-table :test 'equal)))
    (dolist (prop result)
      (puthash (intern (car prop))
               (cdr prop)
               hash))
    hash))

(provide 'editorconfig-core)

;;; editorconfig-core.el ends here
