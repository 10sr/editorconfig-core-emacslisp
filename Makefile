emacs ?= emacs

project_root := ${PWD}

tests_ert_el := $(wildcard tests-ert/*.el)

el := $(wildcard *.el)
elc := $(el:%.el=%.elc)


.PHONY: check check-ert build clean

all:

check: build check-ert tests/CMakeLists.txt lib/editorconfig-fnmatch.el
	cd ${project_root}/tests && cmake -DEDITORCONFIG_CMD="${project_root}/bin/editorconfig-el" .
	cd ${project_root}/tests && EMACS=${emacs} EDITORCONFIG_CORE_LIBRARY_PATH="${project_root}:${project_root}/lib" ctest .

check-ert: $(tests_ert_el) lib/editorconfig-fnmatch.el
	$(emacs) -batch -Q -L . -L lib/ \
		--eval "(require 'ert) (setq debug-on-error t)" \
		$(tests_ert_el:%=-l "%") \
		-f ert-run-tests-batch-and-exit

tests/CMakeLists.txt:
	git submodule update --init

lib/editorconfig-fnmatch.el:
	mkdir -p lib
	curl -sSL https://github.com/10sr/editorconfig-fnmatch-el/raw/master/editorconfig-fnmatch.el >$@

build: $(elc) lib/editorconfig-fnmatch.el

$(elc): %.elc: %.el
	$(emacs) -batch -Q -L . -L lib/ -f batch-byte-compile $<


clean:
	$(RM) lib/editorconfig-fnmatch.el $(elc)
