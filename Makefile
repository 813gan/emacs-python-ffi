PYTHON := "python3"
ifeq ("$(EMACS)","")
EMACS := emacs
endif


.PHONY: all clean test_module_assertions test

all: emacspy.so

emacspy.c: emacspy.pyx
	cython3 emacspy.pyx

# https://github.com/grisha/mod_python/issues/81#issuecomment-551655070
emacspy.so: BLDLIBRARY=$(shell ${PYTHON} -c \
	 'import sysconfig; print(sysconfig.get_config_var("BLDLIBRARY"))')
emacspy.so: PKGCONFIG_PATH=$(shell ${PYTHON} -c \
	 'import sysconfig; print(sysconfig.get_config_var("LIBPC"))')
emacspy.so: LIBPYTHON_NAME=$(shell ${PYTHON} -c \
	 'import sysconfig; print(sysconfig.get_config_var("LDLIBRARY"))')
emacspy.so: emacspy.c stub.c subinterpreter.c
	gcc -fPIC -g -DCYTHON_FAST_THREAD_STATE=0 -DCYTHON_PEP489_MULTI_PHASE_INIT=0 \
		-Wall -fstack-protector -fstack-clash-protection -fcf-protection \
		-D_FORTIFY_SOURCE=2 -ftrapv \
		emacspy.c stub.c \
		${BLDLIBRARY} -DLIBPYTHON_NAME=$(LIBPYTHON_NAME) \
		-shared $(shell pkg-config --cflags --libs $(PKGCONFIG_PATH)"/python3.pc") \
		-o emacspy.so

clean:
	rm -vf emacspy.c emacspy.so

test: all
	${EMACS} --batch -l tests/test.el

test_module_assertions: emacspy.so
	${EMACS} --batch --module-assertions --eval \
		'(progn (add-to-list '\''load-path ".") (load "emacspy"))'
