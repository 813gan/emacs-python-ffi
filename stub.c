#include <Python.h>
#include <dlfcn.h>

#include "emacs-module.h"

int emacs_module_init_py(void *runtime);
void PyInit_emacspy_module(void);
void init_interpreter_list(void);

int emacs_module_init(struct emacs_runtime *runtime) {
	// https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html#index-emacs_005fmodule_005finit-1
	if ((long unsigned int)runtime->size < sizeof(*runtime)) {
		fprintf(stderr, "%s\n",
		    "ERROR: emacs_module_init: emacspy was compiled for newer version of Emacs.");
		return 1;
	}
	if (Py_IsInitialized()) {
		fprintf(stderr, "%s\n",
		    "ERROR: emacs_module_init: emacspy is already loaded.");
		return 2;
	}
	dlopen(LIBPYTHON_NAME, RTLD_LAZY | RTLD_GLOBAL);

	PyConfig config;
	PyStatus status;

	PyConfig_InitPythonConfig(&config);
	status = PyConfig_SetString(&config, &config.home, BASE_PREFIX);
	if (PyStatus_Exception(status)) {
		return 3;
	}
	status = Py_InitializeFromConfig(&config);
	if (PyStatus_Exception(status)) {
		return 4;
	}

	init_interpreter_list();
	PyInit_emacspy_module();
	int result = emacs_module_init_py(runtime);
	PyEval_SaveThread();
	return result;
}
