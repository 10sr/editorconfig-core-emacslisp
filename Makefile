emacs ?= emacs

project_root := ${PWD}

tests_ert_el := $(wildcard tests-ert/*.el)

el := $(wildcard *.el)
elc := $(el:%.el=%.elc)


.PHONY: check check-ert build

all:

check: build check-ert tests/CMakeLists.txt
	cd ${project_root}/tests && cmake -DEDITORCONFIG_CMD=${project_root}/bin/editorconfig-el .
	cd ${project_root}/tests && EMACS=${emacs} EDITORCONFIG_CORE_LIBRARY_PATH=${project_root} ctest .

check-ert: $(tests_ert_el)
	$(emacs) -batch -Q -L . --eval "(require 'ert)" $(^:%=-l "%") \
		-f ert-run-tests-batch-and-exit

tests/CMakeLists.txt:
	git submodule update --init

build: $(elc)

$(elc): %.elc: %.el
	$(emacs) -batch -Q -f batch-byte-compile $<
