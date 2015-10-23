(require 'editorconfig-core)

(ert-deftest editorconfig-core--remove-duplicate ()
  (should (equal (editorconfig-core--remove-duplicate '(("a" . 1) ("b" . 2) ("c" . 3) ("b" . 4)))
                 '(("a" . 1) ("b" . 4) ("c" . 3))))
  (should (equal (editorconfig-core--remove-duplicate '(("a" . 1) ("b" . 2) ("c" . 3)))
                 '(("a" . 1) ("b" . 2) ("c" . 3))))
  (should (equal (editorconfig-core--remove-duplicate nil)
                 nil))
  )


(ert-deftest editorconfig-core--get-handles ()
  (let* ((fixtures (concat default-directory
                          "/tests-ert/fixtures/"))
         (dir (concat fixtures
                      "dir1"))
         (confname "parent.ini")
         (handles (editorconfig-core--get-handles dir
                                                 confname)))
    (should (= 2
               (length handles)))
    (should (editorconfig-core-handle-p (car handles)))
    (should (editorconfig-core-handle-p (cadr handles)))))

(ert-deftest editorconfig-core--version-prior-than ()
  (should (editorconfig-core--version-prior-than "0.8.0"
                                                 "0.9.0"))
  (should-not (editorconfig-core--version-prior-than "0.9.0"
                                                     "0.9.0"))
  (should-not (editorconfig-core--version-prior-than "0.12.0"
                                                     "0.9.0"))
  )
