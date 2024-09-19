PYTHON ?= "python3"
export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK ?= $(shell which cask || echo ${HOME}/.local/bin/cask)
CLANGFORMAT := clang-format

.PHONY: all clean clean_all clean_cask test_module_assertions test_python_api \
	test test_module test_elisp test_formatting test_valgrind

HARDENING_FLAGS := -fstack-protector -fstack-clash-protection -fcf-protection \
	-D_FORTIFY_SOURCE=2 -ftrapv -Wformat=2 -Wjump-misses-init
OPTIMALISATION_FLAGS := -O2

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), Darwin) # Clang does not support some gcc options
GCC_NO_WARN=-Wno-unused-command-line-argument
HARDENING_FLAGS=
ifeq (, $(shell which ${CLANGFORMAT}))
CLANGFORMAT := true # darwin form gh CI have no clang-format
endif
endif

ifeq (, $(shell which pkg-config))
$(error "No pkg-config found.")
endif

all: emacspy_module.so

CASK_DIR := $(shell ${CASK} package-directory)
$(CASK_DIR): Cask
	${CASK} install
	@touch $(CASK_DIR)
cask: $(CASK_DIR)

IS_PYTHON_OLD := $(shell ${PYTHON} -c 'import platform;from packaging import version as v; \
print("-DPYTHON311OLDER") if (v.parse(platform.python_version()) < v.parse("3.12.0")) else exit(0)')

emacspy_module.c: emacspy_module.pyx
	cython emacspy_module.pyx

# https://github.com/grisha/mod_python/issues/81#issuecomment-551655070
emacspy_module.so: BLDLIBRARY=$(shell ${PYTHON} -c \
	 'import sysconfig; print(sysconfig.get_config_var("BLDLIBRARY"))')
emacspy_module.so: PKGCONFIG_PATH=$(shell ${PYTHON} -c \
	 'import sysconfig; print(sysconfig.get_config_var("LIBPC"))')
emacspy_module.so: LIBPYTHON_NAME=$(shell ${PYTHON} -c \
	 'import sysconfig; print(sysconfig.get_config_var("LDLIBRARY"))')
emacspy_module.so: BASE_PREFIX=$(shell ${PYTHON} -c \
	 'import sys; print(sys.base_prefix)')
emacspy_module.so: emacspy_module.c stub.c subinterpreter.c
	gcc -fPIC -g -DCYTHON_FAST_THREAD_STATE=0 -DCYTHON_PEP489_MULTI_PHASE_INIT=0 \
		${IS_PYTHON_OLD} -DBASE_PREFIX=L\"${BASE_PREFIX}\" \
		-Wall -Wextra -Werror ${OPTIMALISATION_FLAGS} ${HARDENING_FLAGS} ${GCC_NO_WARN} \
		emacspy_module.c stub.c \
		${BLDLIBRARY} -DLIBPYTHON_NAME=\"${LIBPYTHON_NAME}\" \
		-shared $(shell pkg-config --cflags --libs $(PKGCONFIG_PATH)"/python3-embed.pc") \
		-o emacspy_module.so

clean:
	rm -vf emacspy_module.c emacspy_module.so
clean_cask:
	rm -vfr .cask
clean_all: clean_cask clean

test: test_module test_elisp test_formatting test_python_api

test_module: cask all
	ulimit -c unlimited; ${CASK} emacs -batch -l tests/prepare-tests.el -l ert -l tests/module-test.el \
		-f ert-run-tests-batch-and-exit

test_elisp: cask all
	ulimit -c unlimited; ${CASK} emacs -batch -l tests/prepare-tests.el -l ert -l tests/elisp-test.el \
		-f ert-run-tests-batch-and-exit

test_python_api: cask all
	ulimit -c unlimited; ${CASK} emacs -batch -l tests/prepare-tests.el -l ert -l tests/python-api-test.el \
		-f ert-run-tests-batch-and-exit

# https://stackoverflow.com/questions/20112989/how-to-use-valgrind-with-python
test_valgrind: OPTIMALISATION_FLAGS=
test_valgrind: clean all .valgrind-python.supp
	PYTHONMALLOC=malloc valgrind --tool=memcheck --suppressions=.valgrind-python.supp \
		--leak-check=full --show-leak-kinds=all \
		${CASK} emacs -batch -l tests/prepare-tests.el -l ert -l tests/test.el \
			-f ert-run-tests-batch-and-exit

.valgrind-python.supp:
	wget -O .valgrind-python.supp \
		"https://raw.githubusercontent.com/python/cpython/main/Misc/valgrind-python.supp"
	echo "WARNING: Read instructions from top of .valgrind-python.supp"
	false

test_formatting:
	${CLANGFORMAT} --dry-run --Werror stub.c subinterpreter.c

test_module_assertions: emacspy_module.so
	${CASK} emacs --batch --module-assertions --eval \
		'(progn (add-to-list '\''load-path ".") (load "emacspy"))'
