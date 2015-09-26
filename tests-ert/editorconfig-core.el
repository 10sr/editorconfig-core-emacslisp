(require 'editorconfig-core)



(ert-deftest test-editorconfig-core-wildcard-match-p ()
  (should (editorconfig-core-wildcard-match-p "a.js"
                                              "*.js"))
  (should (not (editorconfig-core-wildcard-match-p "a.js"
                                                   "*.py")))
  (should (editorconfig-core-wildcard-match-p "/repository/root/a.js"
                                              "/repository/root/*.js"))
  (should (editorconfig-core-wildcard-match-p "/repository/root/a.js"
                                              "*.js"))
  (should (editorconfig-core-wildcard-match-p "/repository/root/sub/a.js"
                                              "*.js"
                                              "/repository/root/"))
  (should (not (editorconfig-core-wildcard-match-p "/repository/root/sub/sub/a.js"
                                                   "*.js"
                                                   "/repository/root/")))
  (should (editorconfig-core-wildcard-match-p "a.js"
                                              "*.js"))
  (should (editorconfig-core-wildcard-match-p "a.js"
                                              "*.js"))
  (should (editorconfig-core-wildcard-match-p "a.js"
                                              "*.js"))
  )
