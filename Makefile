emacs ?= emacs

project_root := ${PWD}

lib_fnmatch := $(project_root)/lib/fnmatch

tests_ert_el := $(wildcard tests-ert/*.el)

el := $(wildcard *.el)
elc := $(el:%.el=%.elc)


.PHONY: all check check-ert build clean

all:

check: build check-ert tests/CMakeLists.txt $(lib_fnmatch)/editorconfig-fnmatch.el
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


clean:
	$(RM) $(elc)
