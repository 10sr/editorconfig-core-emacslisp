[![Build Status](https://travis-ci.org/10sr/editorconfig-core-emacslisp.svg)](https://travis-ci.org/10sr/editorconfig-core-emacslisp)
[![MELPA](https://melpa.org/packages/editorconfig-core-badge.svg)](https://melpa.org/#/editorconfig-core)
[![MELPA Stable](https://stable.melpa.org/packages/editorconfig-core-badge.svg)](https://stable.melpa.org/#/editorconfig-core)



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
[EditorConfig Emacs plugin](https://github.com/editorconfig/editorconfig-emacs).



Installation
-------------


### From MELPA ###

This library is registered to [MELPA repository](http://melpa.org), so just
type `M-x package-install RET editorconfig-core` to install if you already
configured to use this repository.


### Manual Install ###

You need three files to make this library work:

* `editorconfig-core.el`
* `editorconfig-core-handle.el`
* `editorconfig-fnmatch.el`
  * From [here](https://github.com/10sr/editorconfig-fnmatch-el)

Download these files and add the directory to your `load-path`.



Use from EditorConfig Emacs Plugin
----------------------------------

[`editorconfig-emacs`](https://github.com/editorconfig/editorconfig-emacs)
(v0.5 or later) can utilize this library.
Add following lines to your init.el:

    (setq editorconfig-get-properties-function
          'editorconfig-core-get-properties-hash)

This sexp configures `editorconfig-emacs` to call this library when getting
EditorConfig properties instead of the default function
 `editorconfig-get-properties-from-exec`, which invokes external program
like `editorconfig-core-c`.





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
