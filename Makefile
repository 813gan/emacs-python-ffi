LIBPYTHON_NAME := "libpython3.11.so.1"
PYTHON := "python3"

all: emacspy.so

emacspy.c: emacspy.pyx
	cython3 emacspy.pyx

# https://github.com/grisha/mod_python/issues/81#issuecomment-551655070
emacspy.so: BLDLIBRARY=$(shell ${PYTHON} -c \
	 'import distutils.sysconfig; print(distutils.sysconfig.get_config_var("BLDLIBRARY"))')
emacspy.so: emacspy.c stub.c
	gcc -fPIC -g -DCYTHON_FAST_THREAD_STATE=0 -DCYTHON_PEP489_MULTI_PHASE_INIT=0 emacspy.c stub.c \
	 ${BLDLIBRARY} -DLIBPYTHON_NAME=$(LIBPYTHON_NAME) \
	 -shared $(shell pkg-config --cflags --libs python3) -o emacspy.so

clean:
	rm -vf emacspy.c emacspy.so
