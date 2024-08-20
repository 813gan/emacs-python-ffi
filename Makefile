PYTHON := "python3"
ifeq ("$(EMACS)","")
EMACS := emacs
endif
CLANGFORMAT := clang-format

HARDENING_FLAGS := -fstack-protector -fstack-clash-protection -fcf-protection \
	-D_FORTIFY_SOURCE=2 -ftrapv
OPTIMALISATION_FLAGS := -O2

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), Darwin) # Clang does not support some gcc options
GCC_NO_WARN=-Wno-unused-command-line-argument
HARDENING_FLAGS=
ifeq (, $(shell which ${CLANGFORMAT}))
CLANGFORMAT := true # darwin form gh CI have no clang-format
endif
endif

.PHONY: all clean test_module_assertions test test_ert test_formatting test_valgrind

all: emacspy.so

emacspy.c: emacspy.pyx
	cython emacspy.pyx

# https://github.com/grisha/mod_python/issues/81#issuecomment-551655070
emacspy.so: BLDLIBRARY=$(shell ${PYTHON} -c \
	 'import sysconfig; print(sysconfig.get_config_var("BLDLIBRARY"))')
emacspy.so: PKGCONFIG_PATH=$(shell ${PYTHON} -c \
	 'import sysconfig; print(sysconfig.get_config_var("LIBPC"))')
emacspy.so: LIBPYTHON_NAME=$(shell ${PYTHON} -c \
	 'import sysconfig; print(sysconfig.get_config_var("LDLIBRARY"))')
emacspy.so: emacspy.c stub.c subinterpreter.c
	gcc -fPIC -g -DCYTHON_FAST_THREAD_STATE=0 -DCYTHON_PEP489_MULTI_PHASE_INIT=0 \
		-Wall -Wextra -Werror ${OPTIMALISATION_FLAGS} ${HARDENING_FLAGS} ${GCC_NO_WARN} \
		emacspy.c stub.c \
		${BLDLIBRARY} -DLIBPYTHON_NAME=$(LIBPYTHON_NAME) \
		-shared $(shell pkg-config --cflags --libs $(PKGCONFIG_PATH)"/python3-embed.pc") \
		-o emacspy.so

clean:
	rm -vf emacspy.c emacspy.so

test: test_ert test_formatting

test_ert: all
	${EMACS} -batch -l tests/prepare-tests.el -l ert -l tests/test.el \
		-f ert-run-tests-batch-and-exit

test_formatting:
	${CLANGFORMAT} --dry-run --Werror stub.c subinterpreter.c

test_module_assertions: emacspy.so
	${EMACS} --batch --module-assertions --eval \
		'(progn (add-to-list '\''load-path ".") (load "emacspy"))'

# https://stackoverflow.com/questions/20112989/how-to-use-valgrind-with-python
test_valgrind: OPTIMALISATION_FLAGS=
test_valgrind: clean all
	valgrind --tool=memcheck --leak-check=full \
		${EMACS} -batch -l tests/prepare-tests.el -l ert -l tests/test.el \
			-f ert-run-tests-batch-and-exit
