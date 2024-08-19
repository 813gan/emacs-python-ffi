#include <sys/queue.h>

#include <Python.h>
#include <assert.h>
#include <string.h>

#define MAX_INTERPRETER_NAME_LEN 100

#define SUBINTERPRETER_SWITCH                                                       \
	struct interpr *sub_interpreter = get_interpreter(interpreter_name);        \
	PyGILState_STATE gil = PyGILState_Ensure();                                 \
	if (NULL == sub_interpreter) {                                              \
		PyErr_Format(PyExc_KeyError, "Subinterpreter '%s' does not exist.", \
		    interpreter_name);                                              \
		PyGILState_Release(gil);                                            \
		return PyErr_GetRaisedException();                                  \
	}                                                                           \
	PyThreadState *orig_tstate = PyThreadState_Get();                           \
	PyThreadState_Swap(sub_interpreter->python_interpreter);                    \
	PyObject *ret = NULL;                                                       \
	PyObject *exception = NULL;

#define SUBINTERPRETER_RETURN            \
	PyThreadState_Swap(orig_tstate); \
	PyGILState_Release(gil);         \
	assert(ret || exception);        \
	if (NULL == exception) {         \
		return ret;              \
	} else {                         \
		return exception;        \
	}

// man 3 list
struct interpr {
	PyThreadState *python_interpreter;
	PyObject *main_module;
	char *name;
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

void make_interpreter(char *interpreter_name) {
	unsigned int name_len = strnlen(interpreter_name, MAX_INTERPRETER_NAME_LEN) + 1;
	char *name = malloc(name_len);
	assert(name);
	strncpy(name, interpreter_name, name_len);

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
	if (NULL != maybe_existing_interpreter) {
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
	PyObject *main_module = PyImport_AddModule("__main__");
	PyThreadState_Swap(orig_tstate);
	PyGILState_Release(gil);

	struct interpr *new_interpreter = malloc(sizeof(struct interpr));
	assert(new_interpreter);
	new_interpreter->name = name;
	new_interpreter->python_interpreter = tstate;
	new_interpreter->main_module = main_module;
	LIST_INSERT_HEAD(&head, new_interpreter, entries);

	return;
}

PyObject *destroy_subinterpreter(char *interpreter_name) {
	SUBINTERPRETER_SWITCH;

	Py_EndInterpreter(sub_interpreter->python_interpreter);
	free(sub_interpreter->name);
	LIST_REMOVE(sub_interpreter, entries);
	free(sub_interpreter);

	ret = Py_True;
	SUBINTERPRETER_RETURN;
}

PyObject *import_module(char *interpreter_name, PyObject *name, PyObject *as) {
	SUBINTERPRETER_SWITCH;
	PyObject *global_dict = PyModule_GetDict(sub_interpreter->main_module);
	ret = Py_True;

	PyObject *module = PyImport_Import(name); // New reference
	exception = PyErr_GetRaisedException();
	if (exception)
		goto finish;

	PyObject_SetItem(global_dict, as, module);
	exception = PyErr_GetRaisedException();

finish:
	Py_XDECREF(module);
	SUBINTERPRETER_RETURN;
}

PyObject *run_string(char *interpreter_name, char *string, PyObject *target_name) {
	SUBINTERPRETER_SWITCH;
	PyObject *global_dict = PyModule_GetDict(sub_interpreter->main_module);
	PyObject *local_dict = PyDict_New();

	PyObject *obj = PyRun_String(string, Py_eval_input, global_dict, local_dict);
	exception = PyErr_GetRaisedException();
	if (exception)
		goto finish;

	if (PyUnicode_GetLength(target_name) > 0) {
		ret = Py_True;
		PyObject_SetItem(global_dict, target_name, obj);
		exception = PyErr_GetRaisedException();
	} else {
		ret = obj;
	}
finish:
	SUBINTERPRETER_RETURN;
}

PyObject *call_method(char *interpreter_name, PyObject *obj_name, PyObject *method_name,
    PyObject *kwnames, PyObject *target_name, PyObject *args_pylist) {
	SUBINTERPRETER_SWITCH;
	PyObject *global_dict = PyModule_GetDict(sub_interpreter->main_module);
	Py_ssize_t nargs = PyList_Size(args_pylist);
	size_t nargsf = 1 + PyList_Size(args_pylist); // TODO sign to unsign conversion??
	size_t size_obj_args = nargsf * sizeof(PyObject);
	PyObject **obj_with_args = malloc(size_obj_args);
	assert(obj_with_args);
	obj_with_args[0] = PyObject_GetItem(global_dict, obj_name); // New reference

	PyObject *obj = NULL;

	if (NULL == obj_with_args[0]) {
		PyErr_SetObject(PyExc_KeyError, obj_name);
		exception = PyErr_GetRaisedException();
		goto finish;
	}

	for (unsigned int i = 0; i < nargs; ++i) {
		obj_with_args[1 + i] = PyList_GetItem(args_pylist, i);
		assert(obj_with_args[1 + i]);
	}

	obj = PyObject_VectorcallMethod(method_name, obj_with_args, nargsf, kwnames);
	exception = PyErr_GetRaisedException();
	if (exception)
		goto finish;

	if (PyUnicode_GetLength(target_name) > 0) {
		ret = Py_True;
		PyObject_SetItem(global_dict, target_name, obj);
		exception = PyErr_GetRaisedException();
	} else {
		ret = obj;
	}
finish:
	Py_XDECREF(obj_with_args[0]);
	free(obj_with_args);
	SUBINTERPRETER_RETURN;
}

PyObject *call_function(char *interpreter_name, PyObject *callable_name, PyObject *target_name,
    PyObject *args_pylist) {
	SUBINTERPRETER_SWITCH;
	PyObject *global_dict = PyModule_GetDict(sub_interpreter->main_module);
	PyObject *callable = PyObject_GetItem(global_dict, callable_name); // New reference
	PyObject *obj = NULL;

	if (NULL == callable) {
		PyObject *builtins_name = PyUnicode_FromString("__builtins__");
		PyObject *builtins = PyObject_GetItem(global_dict, builtins_name);
		callable = PyObject_GetAttr(builtins, callable_name);
	}

	exception = PyErr_GetRaisedException();
	if (exception)
		goto finish;

	assert(callable);

	obj = PyObject_Call(callable, args_pylist, NULL);
	exception = PyErr_GetRaisedException();
	if (exception)
		goto finish;

	if (PyUnicode_GetLength(target_name) > 0) {
		ret = Py_True;
		PyObject_SetItem(global_dict, target_name, obj);
		exception = PyErr_GetRaisedException();
	} else {
		ret = obj;
	}

finish:
	Py_XDECREF(callable);
	SUBINTERPRETER_RETURN;
}

PyObject *get_global_variable(char *interpreter_name, PyObject *var_name) {
	SUBINTERPRETER_SWITCH;
	PyObject *global_dict = PyModule_GetDict(sub_interpreter->main_module);
	ret = PyObject_GetItem(global_dict, var_name); // New reference
	exception = PyErr_GetRaisedException();

	SUBINTERPRETER_RETURN;
}

PyObject *get_object_attr(char *interpreter_name, PyObject *obj_name, PyObject *attr_name,
    PyObject *target_name) {
	SUBINTERPRETER_SWITCH;
	PyObject *global_dict = PyModule_GetDict(sub_interpreter->main_module);
	PyObject *obj = NULL;

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
	SUBINTERPRETER_RETURN;
}

PyObject *set_global(char *interpreter_name, PyObject *obj, PyObject *as) {
	SUBINTERPRETER_SWITCH;
	PyObject *global_dict = PyModule_GetDict(sub_interpreter->main_module);
	PyObject_SetItem(global_dict, as, obj);
	exception = PyErr_GetRaisedException();
	ret = Py_NewRef(as);

	SUBINTERPRETER_RETURN;
}
