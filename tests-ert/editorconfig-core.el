(require 'editorconfig-core)

(ert-deftest editorconfig-core--merge-two-properties ()
  (should (equal (editorconfig-core--merge-two-properties '(("a" . "1") ("b" . "1"))
                                                          nil)
                 '(("a" . "1") ("b" . "1"))))
  (should (equal (editorconfig-core--merge-two-properties nil
                                                          '(("a" . "1") ("b" . "1")))
                 '(("a" . "1") ("b" . "1"))))
  (should (equal (editorconfig-core--merge-two-properties '(("a" . "1") ("b" . "1"))
                                                          '(("a" . "2") ("c" . "2")))
                 '(("a" . "2") ("b" . "1") ("c" . "2"))))
  (should (equal (editorconfig-core--merge-two-properties '(("a" . "1") ("b" . "1") ("c" . "1"))
                                                          '(("a" . "2") ("c" . "2")))
                 '(("a" . "2") ("b" . "1") ("c" . "2"))))
  (should (equal (editorconfig-core--merge-two-properties '(("a" . "1") ("b" . "1") ("c" . "1"))
                                                          '(("b" . "2") ("d" . "2")))
                 '(("a" . "1") ("b" . "2") ("c" . "1") ("d" . "2"))))
  )

(ert-deftest editorconfig-core--merge-properties ()
  (should (equal (editorconfig-core--merge-properties '(("a" . "1") ("c" . "1")))
                 '(("a" . "1") ("c" . "1"))))
  (should (equal (editorconfig-core--merge-properties '(("a" . "1") ("c" . "1"))
                                                      '(("a" . "2") ("b" . "2")))
                 '(("a" . "2") ("c" . "1") ("b" . "2"))))
  (should (equal (editorconfig-core--merge-properties '(("a" . "1") ("c" . "1"))
                                                      '(("a" . "2") ("b" . "2"))
                                                      '(("d" . "3") ("a" . "3")))
                 '(("a" . "3") ("c" . "1") ("b" . "2") ("d" . "3"))))
  )


(ert-deftest editorconfig-core-get-handles ()
  (let* ((fixtures (concat default-directory
                          "/tests-ert/fixtures/"))
         (dir (concat fixtures
                      "dir1"))
         (confname "parent.ini")
         (handles (editorconfig-core-get-handles dir
                                                 confname)))
    (should (= 2
               (length handles)))
    (should (editorconfig-core-handle-p (car handles)))
    (should (editorconfig-core-handle-p (cadr handles)))))
