#include <emacs-module.h>
#include <Python.h>
#include <dlfcn.h>

int emacs_module_init_py(void* runtime);
void PyInit_emacspy(void);
void init_interpreter_list(void);

int emacs_module_init(struct emacs_runtime* runtime) {
    // https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html#index-emacs_005fmodule_005finit-1
    if (runtime->size < sizeof (*runtime)) {
        fprintf(stderr, "%s\n", \
                "ERROR: emacs_module_init: emacspy was compiled for newer version of Emacs.");
        return 1;
    }
    dlopen("#LIBPYTHON_NAME", RTLD_LAZY | RTLD_GLOBAL);
    Py_Initialize();
    init_interpreter_list();
    PyInit_emacspy();
    int result = emacs_module_init_py(runtime);
    PyEval_SaveThread();
    return result;
}
