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
	unsigned int name_len = strnlen(inter_name, MAX_INTERPRETER_NAME_LEN) + 1;
	char *name = malloc(name_len);
	strncpy(name, inter_name, name_len);

	// https://docs.python.org/3/c-api/init.html#c.Py_NewInterpreterFromConfig
	const PyInterpreterConfig config = {
		.use_main_obmalloc = 1,
		.allow_fork = 0,
		.allow_exec = 0,
		.allow_threads = 1,
		.allow_daemon_threads = 0,
		.check_multi_interp_extensions = 1,
		.gil = PyInterpreterConfig_SHARED_GIL,
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

PyObject* import_module(PyObject *name, PyObject *as, char *interpreter_name) {
	struct interpr *sub_interpreter = get_interpreter(interpreter_name);
	PyGILState_STATE gil = PyGILState_Ensure();
	PyThreadState *orig_tstate = PyThreadState_Get();
	PyThreadState_Swap(sub_interpreter->python_interpreter);

	PyObject *global_dict = PyModule_GetDict(sub_interpreter->main_module);
	PyObject *ret = Py_True;

	PyObject *module = PyImport_Import(name); // New reference
	PyObject *exception = PyErr_GetRaisedException();
	if (exception) 
		goto finish;

	PyObject_SetItem(global_dict, as, module);
	exception = PyErr_GetRaisedException();

finish:
	Py_XDECREF(module);
	PyThreadState_Swap(orig_tstate);
	PyGILState_Release(gil);

	assert(ret || exception);
	if (NULL == exception) {
		return ret;
	} else {
		return exception;
	}
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
		return obj;
	} else {
		return exception;
	}
}


PyObject* call_method(PyObject *obj_name, PyObject *method_name, PyObject *args_pylist,	\
		      PyObject *kwnames, char *interpreter_name) {
	struct interpr *sub_interpreter = get_interpreter(interpreter_name);
	PyGILState_STATE gil = PyGILState_Ensure();
	PyThreadState *orig_tstate = PyThreadState_Get();
	PyThreadState_Swap(sub_interpreter->python_interpreter);

	PyObject *global_dict = PyModule_GetDict(sub_interpreter->main_module);

	Py_ssize_t nargs = PyList_Size(args_pylist);
	size_t nargsf = 1 + PyList_Size(args_pylist); // TODO sign to unsign conversion??
	size_t size_obj_args = nargsf * sizeof(PyObject);
	PyObject **obj_with_args = malloc(size_obj_args);
	assert(obj_with_args);
	obj_with_args[0] = PyObject_GetItem(global_dict, obj_name); // New reference

	PyObject *ret = NULL;
	PyObject *exception = NULL;

	if (NULL == obj_with_args[0]) {
		PyErr_SetObject(PyExc_KeyError, obj_name);
		exception = PyErr_GetRaisedException();
		goto finish;
	}

	for (unsigned int i = 0; i < nargs; ++i) {
		obj_with_args[1 + i] = PyList_GetItem(args_pylist, i);
	}

	ret = PyObject_VectorcallMethod(method_name, obj_with_args, nargsf, kwnames);
	exception = PyErr_GetRaisedException();
finish:
	Py_XDECREF(obj_with_args[0]);
	free(obj_with_args);

	PyThreadState_Swap(orig_tstate);
	PyGILState_Release(gil);

	assert(ret || exception);
	if (NULL == exception) {
		return ret;
	} else {
		return exception; // Raised by caller
	}
}

PyObject* call_function (PyObject *callable_name, PyObject *args_pylist, char *interpreter_name) {
	struct interpr *sub_interpreter = get_interpreter(interpreter_name);
	PyGILState_STATE gil = PyGILState_Ensure();
	PyThreadState *orig_tstate = PyThreadState_Get();
	PyThreadState_Swap(sub_interpreter->python_interpreter);

	PyObject *global_dict = PyModule_GetDict(sub_interpreter->main_module);
	PyObject *callable = PyObject_GetItem(global_dict, callable_name); // New reference
	PyObject *exception = NULL;
	PyObject *ret = NULL;

	if (NULL == callable) {
		PyObject *builtins_name = PyUnicode_FromString("__builtins__");
		PyObject *builtins = PyObject_GetItem(global_dict, builtins_name);
		callable = PyObject_GetAttr(builtins, callable_name);
	}

	exception = PyErr_GetRaisedException();
	if (exception)
		goto finish;

	assert(callable);

	ret = PyObject_Call(callable, args_pylist, NULL);
	exception = PyErr_GetRaisedException();

finish:
	Py_XDECREF(callable);
	PyThreadState_Swap(orig_tstate);
	PyGILState_Release(gil);

	assert(ret || exception);
	if (NULL == exception) {
		return ret;
	} else {
		return exception;
	}
}

PyObject* get_global_variable (PyObject *var_name, char *interpreter_name) {
	struct interpr *sub_interpreter = get_interpreter(interpreter_name);
	PyGILState_STATE gil = PyGILState_Ensure();
	PyThreadState *orig_tstate = PyThreadState_Get();
	PyThreadState_Swap(sub_interpreter->python_interpreter);

	PyObject* global_dict = PyModule_GetDict(sub_interpreter->main_module);

	PyObject* obj = PyObject_GetItem(global_dict, var_name); // New reference
	PyObject* exception = PyErr_GetRaisedException();

	PyThreadState_Swap(orig_tstate);
	PyGILState_Release(gil);

	assert(obj || exception);
	if (NULL==exception) {
		return obj;
	} else {
		return exception; // Raised by caller
	}
}

PyObject* get_object_attr(char *interpreter_name, PyObject *obj_name, \
			   PyObject *attr_name, PyObject *target_name) {
	struct interpr *sub_interpreter = get_interpreter(interpreter_name);
	PyGILState_STATE gil = PyGILState_Ensure();
	PyThreadState *orig_tstate = PyThreadState_Get();
	PyThreadState_Swap(sub_interpreter->python_interpreter);

	PyObject *obj = NULL;
	PyObject *ret = NULL;
	PyObject *exception = NULL;

	PyObject *global_dict = PyModule_GetDict(sub_interpreter->main_module);
	PyObject *holding_obj = PyObject_GetItem(global_dict, obj_name); // New reference
	if (NULL == holding_obj) {
		PyErr_SetObject(PyExc_KeyError, holding_obj);
		exception = PyErr_GetRaisedException();
		goto finish;
	}

	obj = PyObject_GetAttr(holding_obj, attr_name); // New reference
	if (NULL == obj) {
		PyErr_SetObject(PyExc_AttributeError, attr_name);
		exception = PyErr_GetRaisedException();
		goto finish;
	}

	if (PyUnicode_GetLength(target_name) > 0) {
		ret = Py_True;
		PyObject_SetItem(global_dict, target_name, obj);
		exception = PyErr_GetRaisedException();
	} else {
		ret = obj;
	}

finish:
	Py_XDECREF(holding_obj);

	PyThreadState_Swap(orig_tstate);
	PyGILState_Release(gil);

	assert(ret || exception);
	if (NULL == exception) {
		return ret;
	} else {
		return exception;
	}
}

PyObject* set_global(char *interpreter_name, PyObject *as, PyObject *obj) {
	struct interpr *sub_interpreter = get_interpreter(interpreter_name);
	PyGILState_STATE gil = PyGILState_Ensure();
	PyThreadState *orig_tstate = PyThreadState_Get();
	PyThreadState_Swap(sub_interpreter->python_interpreter);

	PyObject* global_dict = PyModule_GetDict(sub_interpreter->main_module);

	PyObject_SetItem(global_dict, as, obj);
	PyObject* exception = PyErr_GetRaisedException();
	PyObject* ret = Py_True;

	PyThreadState_Swap(orig_tstate);
	PyGILState_Release(gil);

	assert(obj || exception);
	if (NULL==exception) {
		return ret;
	} else {
		return exception; // Raised by caller
	}
}
