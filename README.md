[![Build Status](https://travis-ci.org/10sr/editorconfig-core-emacslisp.svg)](https://travis-ci.org/10sr/editorconfig-core-emacslisp)



editorconfig-core-emacslisp
============================

[EditorConfig](http://editorconfig.org/) Core library written purely in Emacs
Lisp.


What is This?
---------------

This library is one implementation of EditorConfig Core, which parses
`.editorconfig` files and returns properties for given files.
This can be used in place of, for example,
[editorconfig-core-c](https://github.com/editorconfig/editorconfig-core-c).

This library is *not* an editor plugin and does not configure Emacs for editing
 the files: this should be done with
[editorconfig-emacs](https://github.com/editorconfig/editorconfig-emacs).


Functions
--------

* `editorconfig-core-get-properties (&optional file confname confversion)`

  Get EditorConfig properties for FILE.

  If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than `.editorconfig`.
If need to specify config format version, give CONFVERSION.

  This functions returns alist of properties. Each element will look like
`(KEY . VALUE)`.

* `editorconfig-core-get-properties-hash (&optional file confname confversion)`

  Get EditorConfig properties for FILE.

  This function is almost same as `editorconfig-core-get-properties`, but
returns hash object instead.



Run Test
---------

Issue

    make check

to conduct all tests (update submodules for tests and run defined tests).

License
---------

This software is licensed under MIT License. See `LICENSE` for details.
