#include <sys/queue.h>

#include <Python.h>
#include <assert.h>
#include <string.h>

#define MAX_INTERPRETER_NAME_LEN 100

#ifdef PYTHON311OLDER
#define PyErr_GetRaisedException()                                   \
	({                                                           \
		PyObject *type, *value, *traceback;                  \
		PyErr_Fetch(&type, &value, &traceback);              \
		PyErr_NormalizeException(&type, &value, &traceback); \
		value;                                               \
	})
#endif

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
	PyObject *exception = PyErr_GetRaisedException();                           \
	if (exception)                                                              \
		return exception;

#define SETUP_RET                                                \
	if (PyUnicode_GetLength(target_name) > 0) {              \
		ret = Py_True;                                   \
		PyObject_SetItem(global_dict, target_name, obj); \
		exception = PyErr_GetRaisedException();          \
		Py_DECREF(obj);                                  \
	} else {                                                 \
		ret = obj;                                       \
	}

#define SUBINTERPRETER_RETURN            \
	PyThreadState_Swap(orig_tstate); \
	PyGILState_Release(gil);         \
	assert(ret || exception);        \
	if (NULL == exception) {         \
		return ret;              \
	} else {                         \
		Py_XDECREF(ret);         \
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

PyObject *make_interpreter(char *interpreter_name) {
	PyGILState_STATE gil = PyGILState_Ensure();
	PyThreadState *tstate = NULL;
	PyThreadState *orig_tstate = PyThreadState_Get();

	struct interpr *maybe_existing_interpreter = get_interpreter(interpreter_name);
	if (NULL != maybe_existing_interpreter) {
		PyGILState_Release(gil);
		return Py_True;
	}

	unsigned int name_len = strnlen(interpreter_name, MAX_INTERPRETER_NAME_LEN) + 1;
	char *name = malloc(name_len);
	assert(name);
	strncpy(name, interpreter_name, name_len);

	tstate = Py_NewInterpreter();
	if (NULL == tstate) {
		return Py_False;
	}
	PyThreadState_Swap(tstate);

	PyObject *main_module = PyImport_AddModule("__main__");
	PyThreadState_Swap(orig_tstate);
	PyGILState_Release(gil);

	struct interpr *new_interpreter = malloc(sizeof(struct interpr));
	assert(new_interpreter);
	new_interpreter->name = name;
	new_interpreter->python_interpreter = tstate;
	new_interpreter->main_module = main_module;
	LIST_INSERT_HEAD(&head, new_interpreter, entries);

	return Py_True;
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

PyObject *list_subinterpreters() {
	struct interpr *iter;
	PyObject *ret_list = PyList_New(0);

	LIST_FOREACH(iter, &head, entries) {
		PyObject *interpreter_name = PyUnicode_FromString(iter->name);
		PyList_Append(ret_list, interpreter_name);
		Py_DECREF(interpreter_name);
	}
	return ret_list;
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

	SETUP_RET;
finish:
	SUBINTERPRETER_RETURN;
}

PyObject *call_py(char *interpreter_name, PyObject *obj_name, PyObject *method_name,
    PyObject *target_name, PyObject *args_pytuple, PyObject *kwargs_pydict) {
	SUBINTERPRETER_SWITCH;
	PyObject *global_dict = PyModule_GetDict(sub_interpreter->main_module);
	PyObject *call_obj = PyObject_GetItem(global_dict, obj_name); // New reference
	PyObject *obj = NULL;
	PyObject *method = NULL;

	if (NULL == call_obj) {
		PyErr_Clear();
		PyObject *builtins_name = PyUnicode_FromString("__builtins__");
		PyObject *builtins = PyObject_GetItem(global_dict, builtins_name);
		Py_DECREF(builtins_name);
		call_obj = PyObject_GetAttr(builtins, obj_name); // New reference
	}

	exception = PyErr_GetRaisedException();
	if (exception)
		goto finish;

	assert(call_obj);

	if (1 != PyObject_IsTrue(kwargs_pydict)) {
		kwargs_pydict = NULL;
	}

	if (1 == PyObject_IsTrue(method_name)) {
		method = PyObject_GetAttr(call_obj, method_name); // New reference

		exception = PyErr_GetRaisedException();
		if (exception)
			goto finish;

		if (0 == PyCallable_Check(method)) {
			PyErr_SetObject(PyExc_ValueError, method_name);
			exception = PyErr_GetRaisedException();
			goto finish;
		}
		obj = PyObject_Call(method, args_pytuple, kwargs_pydict); // New reference
	} else {
		obj = PyObject_Call(call_obj, args_pytuple, kwargs_pydict); // New reference
	}
	exception = PyErr_GetRaisedException();
	if (exception)
		goto finish;

	SETUP_RET;
finish:
	Py_XDECREF(call_obj);
	Py_XDECREF(method);
	SUBINTERPRETER_RETURN;
}

PyObject *get_global_variable(char *interpreter_name, PyObject *var_name) {
	SUBINTERPRETER_SWITCH;
	PyObject *global_dict = PyModule_GetDict(sub_interpreter->main_module);
	// Doc don't mention it but it will raise exception on wrong key
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

	SETUP_RET;
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
