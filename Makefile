emacs ?= emacs

project_root := ${PWD}

.PHONY: check check-ert build

check: build check-ert tests/CMakeLists.txt
	cd ${project_root}/tests && cmake -DEDITORCONFIG_CMD=${project_root}/bin/editorconfig-el .
	cd ${project_root}/tests && EMACS=${emacs} EDITORCONFIG_CORE_LIBRARY_PATH=${project_root} ctest .

check-ert: build

tests/CMakeLists.txt:
	git submodule update --init

build:
