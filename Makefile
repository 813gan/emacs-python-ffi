PYTHON ?= "python3"
export EMACS ?= $(shell which emacs 2>/dev/null)
export CASK ?= $(shell which cask || echo ${HOME}/.local/bin/cask )
CLANGFORMAT := clang-format

C_VERSION=-std=c11

.PHONY: all clean clean_all clean_cask test_c_g \
	test test_module test_elisp test_formatting test_valgrind

HARDENING_FLAGS := -fstack-protector -fstack-clash-protection -fcf-protection \
	-D_FORTIFY_SOURCE=2 -ftrapv -Wformat=2 -Wjump-misses-init -Wnull-dereference
OPTIMALISATION_FLAGS ?= -O2

## Darwin compatibility
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), Darwin) # Clang does not support some gcc options
GCC_NO_WARN=-Wno-unused-command-line-argument
HARDENING_FLAGS=
ifeq (, $(shell which ${CLANGFORMAT}))
CLANGFORMAT := true # darwin form gh CI have no clang-format
endif
endif

ifeq (, $(shell pkg-config --help))
$(error "pkg-config not found.")
endif

.DEFAULT_GOAL := python-ffi-module.so

all: python-ffi-module.so emacs-python-ffi.html

CASK_DIR := $(shell ${CASK} package-directory)
$(CASK_DIR): Cask
	${CASK} install
	@touch $(CASK_DIR)
cask: $(CASK_DIR)

IS_PYTHON_OLD := $(shell ${PYTHON} -c 'from sys import version_info; \
print("-DPYTHON311OLDER") if (version_info < (3, 12)) else exit(0)')

# https://github.com/grisha/mod_python/issues/81#issuecomment-551655070
python-ffi-module.so: BLDLIBRARY=$(shell ${PYTHON} -c \
	 'import sysconfig; print(sysconfig.get_config_var("BLDLIBRARY"))')
python-ffi-module.so: PKGCONFIG_PATH=$(shell ${PYTHON} -c \
	 'import sysconfig; print(sysconfig.get_config_var("LIBPC"))')
python-ffi-module.so: LIBPYTHON_NAME=$(shell ${PYTHON} -c \
	 'import sysconfig; print(sysconfig.get_config_var("LDLIBRARY"))')
python-ffi-module.so: BASE_PREFIX=$(shell ${PYTHON} -c \
	 'import sys; print(sys.base_prefix)')
python-ffi-module.so: python-ffi.c python-ffi-functions.c
	gcc ${C_VERSION} -fPIC -g \
		${IS_PYTHON_OLD} -DBASE_PREFIX=L\"${BASE_PREFIX}\" \
		-Wall -Wextra -Werror ${OPTIMALISATION_FLAGS} ${HARDENING_FLAGS} ${GCC_NO_WARN} \
		python-ffi-functions.c python-ffi.c \
		${BLDLIBRARY} -DLIBPYTHON_NAME=\"${LIBPYTHON_NAME}\" \
		-shared $(shell pkg-config --cflags --libs $(PKGCONFIG_PATH)"/python3-embed.pc") \
		-o python-ffi-module.so

clean:
	rm -vf python-ffi-module.so emacs-python-ffi.texi emacs-python_ffi.info emacs-python-ffi.html
clean_cask:
	rm -vfr .cask
clean_all: clean_cask clean

test: test_module test_elisp test_formatting # test_c_g dont work in CI for some reason

test_module: cask python-ffi-module.so
	ulimit -c unlimited; ${CASK} emacs --module-assertions -batch -l ert \
		-l tests/module-test.el -f ert-run-tests-batch-and-exit

test_elisp: cask python-ffi-module.so
	ulimit -c unlimited; ${CASK} emacs --module-assertions -batch -l ert \
		-l tests/elisp-test.el -f ert-run-tests-batch-and-exit

# https://stackoverflow.com/questions/20112989/how-to-use-valgrind-with-python
test_valgrind: OPTIMALISATION_FLAGS=
test_valgrind: clean python-ffi-module.so .valgrind-python.supp
	PYTHONMALLOC=malloc ${CASK} exec \
		valgrind --tool=memcheck --suppressions=.valgrind-python.supp \
		--leak-check=full --show-leak-kinds=all \
		${EMACS} -batch -l ert -l tests/module-test.el -f ert-run-tests-batch-and-exit

.valgrind-python.supp:
	wget -O .valgrind-python.supp \
		"https://raw.githubusercontent.com/python/cpython/main/Misc/valgrind-python.supp"
	echo "WARNING: Read instructions from top of .valgrind-python.supp"
	false

test_formatting:
	${CLANGFORMAT} --dry-run --Werror python-ffi.c python-ffi-functions.c datatypes.h

test_c_g: cask python-ffi-module.so
	bash -x tests/test_c_g.sh

emacs_python_ffi.texi: README.org
	emacs --batch README.org -l org -f org-texinfo-export-to-texinfo --kill

emacs-python-ffi.info: emacs-python-ffi.texi
	makeinfo --no-split emacs-python-ffi.texi

emacs-python-ffi.html: emacs-python-ffi.texi
	makeinfo --html --no-split -o emacs-python-ffi.html emacs-python-ffi.texi
