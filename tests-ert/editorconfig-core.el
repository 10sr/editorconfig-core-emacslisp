(require 'editorconfig-core)

(ert-deftest test-editorconfig-core-wildcard-match-p ()
  (let ((repository-root "/repository/root")
        (cases-t
         '(("a.js" "a.js")
           ("/repository/root/a.js" "a.js")
           ("/repository/root/sub/a.js" "a.js")
           ("/repository/root/a.js" "/repository/root/a.js")

           ("a.js" "*.js")
           ("/repository/root/a.js" "/repository/root/*.js")
           ("/repository/root/a.js" "*.js")
           ("/repositroy/root/sub/a.js" "*.js")
           ("/repositroy/root/sub/a.js" "sub/*.js")
           ("/repository/root/sub/dir/a.js" "sub/**.js")

           ("a.js" "?.js")
           ("abc.js" "a?c.js")
           ("/repository/root/a.js" "/repository/root/?.js")

           ("a.js" "[abc].js")
           ("b.js" "[abc].js")
           ("ab.js" "[abc]b.js")
           ("/repository/root/a.js" "/repository/root/[abc].js")

           ("d.js" "[^abc].js")
           ("db.js" "[^abc]b.js")
           ("/repository/root/d.js" "/repository/root/[^abc].js")

           ("/repositroy/root/a.js" "*.{py,js}")
           ("/repositroy/root/a.py" "*.{py,js}")
           ("/repository/root/sub/a.py" "*.{py,js}")
           ("/repository/root/sub/a.py" "**.{py,js}")
           ("/repository/root/sub/dir/a.py" "sub/**.{py,js}")
           ))
        (cases-nil
         '(("a.js" "b.js")
           ("a.js" "*.py")
           ("/repository/a.js" "b.js")
           ("/repository/a.js" "/repository/root/*.js")
           ("/repository/root/sub/a.js" "/repository/root/*.js")
           ("/repository/root/sub/dir/a.js" "sub/*.js")
           ("/repository/root/a.js" "lib/*.js")
           ("/repository/root/a.js" "lib/**.js")

           ("ab.js" "?.js")
           ("ab.js" "?a.js")
           ("/repository/root/ab.js" "/repository/root/?.js")
           ("/repository/root/ab.js" "/repository/root/?a.js")

           ("d.js" "[abc].js")
           ("db.js" "[abc]b.js")
           ("/repository/root/d.js" "/repository/root/[abc].js")

           ("a.js" "[^abc].js")
           ("ab.js" "[^abc]b.js")
           ("/repository/root/a.js" "/repository/root/[^abc].js")

           ("/repository/root/a.py" "sub/*.{py,js}")
           ("/repository/root/sub/dir/a.py" "sub/*.{py,js}")
           ())))
    (dolist (args cases-t)
      (message "-> %S should non-nil"
               `(editorconfig-core-wildcard-match-p ,@args))
      (should (apply 'editorconfig-core-wildcard-match-p
                     `(,@args repository-root))))
    (dolist (args cases-nil)
      (message "-> %S should nil"
               `(editorconfig-core-wildcard-match-p ,@args))
      (should-not (apply 'editorconfig-core-wildcard-match-p
                         `(,@args repository-root)))))
  )
