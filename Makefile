emacs ?= emacs

project_root := ${PWD}

lib_fnmatch := $(project_root)/lib/fnmatch

tests_ert_el := $(wildcard tests-ert/*.el)

el := $(wildcard *.el)
elc := $(el:%.el=%.elc)


.PHONY: all check check-ert build clean check-info

all:

check: build check-ert check-info tests/CMakeLists.txt $(lib_fnmatch)/editorconfig-fnmatch.el
	cd ${project_root}/tests && cmake -DEDITORCONFIG_CMD="$(project_root)/bin/editorconfig-el" .
	cd ${project_root}/tests && EMACS_BIN=$(emacs) EDITORCONFIG_CORE_LIBRARY_PATH="$(project_root):$(lib_fnmatch)" ctest --output-on-failure .

check-ert: $(tests_ert_el) $(lib_fnmatch)/editorconfig-fnmatch.el build
	$(emacs) -batch -Q -L $(project_root) -L $(lib_fnmatch) \
		--eval "(require 'ert) (setq debug-on-error t)" \
		$(tests_ert_el:%=-l "%") \
		-f ert-run-tests-batch-and-exit

tests/CMakeLists.txt:
	git submodule update --init

$(lib_fnmatch)/editorconfig-fnmatch.el:
	mkdir -p lib
	git submodule update --init

build: $(elc)

$(elc): %.elc: %.el $(lib_fnmatch)/editorconfig-fnmatch.el
	$(emacs) -batch -Q -L $(project_root) -L $(lib_fnmatch) -f batch-byte-compile $<



elisp_get_file_package_info := \
	(lambda (f) \
		(with-temp-buffer \
			(insert-file-contents-literally f) \
			(package-buffer-info)))

elisp_print_infos := \
	(mapc \
		(lambda (f) \
			(message \"Loading info: %s\" f) \
			(message \"%S\" (funcall $(elisp_get_file_package_info) f))) \
		command-line-args-left)

check-info: $(el)
	$(emacs) -batch -Q \
		--eval "(require 'package)" \
		--eval "$(elisp_print_infos)" \
		$^


# elisp_install_files := \
# 	(mapc \
# 		(lambda (f) \
# 			(message \"Install file: %s\" f) \
# 			(package-install-file f)) \
# 		command-line-args-left)

# check-install: $(el) $(lib_fnmatch)/editorconfig-fnmatch.el
# 	$(emacs) -batch -Q \
# 		 -L $(project_root) -L $(lib_fnmatch) \
# 		--eval '(setq package-user-dir "$(PWD)/test-install")' \
# 		--eval "(require 'package)" \
# 		--eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.milkbox.net/packages/\"))" \
# 		--eval '(package-initialize)' \
# 		--eval "$(elisp_install_files)" \
# 		$(el)


clean:
	$(RM) $(elc)
