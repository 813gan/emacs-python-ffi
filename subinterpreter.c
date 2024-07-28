#include <Python.h>
#include <sys/queue.h>
#include <string.h>
#include <assert.h>

#define MAX_INTERPRETER_NAME_LEN 100

// man 3 list
struct interpr {
	PyThreadState* python_interpreter;
	PyObject* main_module;
	char* name;
	LIST_ENTRY(interpr) entries;
};

LIST_HEAD(listhead, interpr);
struct listhead head;

void init_interpreter_list() {
	LIST_INIT(&head);
}

struct interpr *get_interpreter(char *name) {
	struct interpr *ret = NULL;
	struct interpr *iter;
	LIST_FOREACH(iter, &head, entries) {
		if (0 == strncmp(iter->name, name, MAX_INTERPRETER_NAME_LEN)) {
			ret = iter;
			break;
		}
	}
	return ret;
}

void make_interpreter(char *inter_name) {
	u_int name_len = strnlen(inter_name, MAX_INTERPRETER_NAME_LEN) + 1;
	char *name = malloc(name_len);
	strncpy(name, inter_name, name_len);

	// https://docs.python.org/3/c-api/init.html#c.Py_NewInterpreterFromConfig
	const PyInterpreterConfig config = {
		.use_main_obmalloc = 0,
		.allow_fork = 0,
		.allow_exec = 0,
		.allow_threads = 1,
		.allow_daemon_threads = 0,
		.check_multi_interp_extensions = 1,
		.gil = PyInterpreterConfig_OWN_GIL,
	};

	PyGILState_STATE gil = PyGILState_Ensure();

	struct interpr *maybe_existing_interpreter = get_interpreter(name);
	if (NULL!=maybe_existing_interpreter) {
		PyGILState_Release(gil);
		return;
	}

	PyThreadState *tstate = PyThreadState_Get();

	PyThreadState *orig_tstate = PyThreadState_Get();
	PyThreadState_Swap(NULL);

	PyStatus status = Py_NewInterpreterFromConfig(&tstate, &config);
	if (PyStatus_Exception(status)) {
		Py_ExitStatusException(status);
	}
	PyObject* main_module = PyImport_AddModule("__main__");
	PyThreadState_Swap(orig_tstate);
	PyGILState_Release(gil);

	struct interpr *new_interpreter = malloc(sizeof(struct interpr));
	new_interpreter->name = name;
	new_interpreter->python_interpreter = tstate;
	new_interpreter->main_module = main_module;
	LIST_INSERT_HEAD(&head, new_interpreter, entries);

	return;
}

void import_module(PyObject *name, PyObject *as, char *interpreter_name) {
	struct interpr *sub_interpreter = get_interpreter(interpreter_name);
	PyGILState_STATE gil = PyGILState_Ensure();
	PyThreadState *orig_tstate = PyThreadState_Get();
	PyThreadState_Swap(sub_interpreter->python_interpreter);

	PyObject* global_dict = PyModule_GetDict(sub_interpreter->main_module);

	PyObject_SetItem(global_dict, as, PyImport_Import(name));

	PyThreadState_Swap(orig_tstate);
	PyGILState_Release(gil);
}

PyObject* run_string(char *string, char *interpreter_name) {
	struct interpr *sub_interpreter = get_interpreter(interpreter_name);
	PyGILState_STATE gil = PyGILState_Ensure();
	PyThreadState *orig_tstate = PyThreadState_Get();
	PyThreadState_Swap(sub_interpreter->python_interpreter);

	PyObject* global_dict = PyModule_GetDict(sub_interpreter->main_module);
	PyObject* local_dict = PyDict_New();

	PyObject* obj = PyRun_String(string, Py_eval_input, global_dict, local_dict);
	PyObject* exception = PyErr_GetRaisedException();

	PyThreadState_Swap(orig_tstate);
	PyGILState_Release(gil);

	assert(obj || exception);
	if (NULL==exception) {
		return Py_NewRef(obj);
	} else {
		return exception;
	}
}
