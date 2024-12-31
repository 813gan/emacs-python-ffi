#include <sys/queue.h>

#include <Python.h>
#include <assert.h>
#include <string.h>

#include "datatypes.h"

#define MAX_INTERPRETER_NAME_LEN 256

struct argument py2c(PyObject *arg) {
	// TODO if exception got raised in middle of list conversion one of list elements will
	// be exception.
	struct argument ret;
	PyObject *exc_type, *exc_value, *exc_traceback;
#ifdef PYTHON311OLDER
	// The value and traceback object may be ‘NULL’ even when the type object is not.
	PyErr_Fetch(&exc_type, &exc_value, &exc_traceback);
	PyErr_NormalizeException(&exc_type, &exc_value, &exc_traceback);
#else
	exc_value = PyErr_GetRaisedException();
#endif
	if (!exc_value)
		assert(arg);
	if (exc_value) {
		ret.type = exception_e;
#ifndef PYTHON311OLDER
		exc_traceback = PyException_GetTraceback(exc_value);
		exc_type = PyObject_Type(exc_value);
#endif
		argument str_exc_value = py2c(PyObject_Str(exc_value));
		argument str_exc_type;
		if (exc_type) {
			str_exc_type = py2c(PyObject_Str(exc_type));
		} else {
			str_exc_type.string.size = 0;
			str_exc_type.string.buf = NULL;
		}

		argument str_exc_tb;
		if (exc_type) {
			str_exc_tb = py2c(PyObject_Str(exc_traceback));
		} else {
			str_exc_tb.string.size = 0;
			str_exc_tb.string.buf = NULL;
		}

		Py_XDECREF(exc_type);
		Py_XDECREF(exc_value);
		Py_XDECREF(exc_traceback);

		ret.exception.type = str_exc_type.string;
		ret.exception.value = str_exc_value.string;
		ret.exception.traceback = str_exc_tb.string;
	} else if (PyBool_Check(arg)) {
		ret.type = boolean_e;
		ret.boolean = (Py_IsTrue(arg) ? true : false);
	} else if (Py_IsNone(arg)) {
		ret.type = boolean_e;
		ret.boolean = false;
	} else if (PyUnicode_Check(arg)) {
		ret.type = string_e;
		Py_ssize_t size;
		const char *pybuf = PyUnicode_AsUTF8AndSize(arg, &size);
		ret.string.size = size;
		ret.string.buf = malloc(ret.string.size); // free in c2elisp/exec/eval // TODO
							  // valgrind complains about this
		memcpy(ret.string.buf, pybuf, ret.string.size);
	} else if (PyLong_Check(arg)) {
		ret.type = integer_e;
		ret.integer = PyLong_AsLongLong(arg);
		if (PyErr_Occurred())
			ret = py2c(NULL); // this feels like hack
	} else if (PyFloat_Check(arg)) {
		ret.type = floating_e;
		ret.floating = PyFloat_AsDouble(arg);
		if (PyErr_Occurred())
			ret = py2c(NULL);
	} else if (PyDict_Check(arg)) {
		ret.type = hash_e;
		argument keys = py2c(PyDict_Keys(arg));
		argument values = py2c(PyDict_Values(arg));
		ret.hash.keys.size = keys.list.size;
		ret.hash.keys.buf = keys.list.buf;
		ret.hash.values.size = values.list.size;
		ret.hash.values.buf = values.list.buf;
	} else if (PyList_Check(arg) || PyTuple_Check(arg)) {
		PyObject *arg_tup = PyTuple_Check(arg) ? arg : PyList_AsTuple(arg);
		Py_ssize_t size = PyTuple_Size(arg_tup);
		PyObject *el;
		ret.type = cons_e;
		ret.list.size = size;
		ret.list.buf = malloc(size * sizeof(argument)); // free in c2elisp
		for (Py_ssize_t i = 0; i < size; ++i) {
			// py2c always do Py_XDECREF so make strong ref from borrowed ref from
			// GetItem
			el = Py_NewRef(PyTuple_GetItem(arg_tup, i));
			ret.list.buf[i] = py2c(el);
		}
		if (!PyTuple_Check(arg))
			Py_XDECREF(arg_tup);
	} else {
		PyErr_Format(PyExc_NotImplementedError, "py2c Unable to convert");
		ret = py2c(NULL); // this feels like hack
	}
	Py_XDECREF(arg);
	return ret;
}

PyObject *c2py(struct argument arg) {
	PyObject *ret = NULL;
	PyObject *k = NULL;
	PyObject *v = NULL;
	switch (arg.type) {
	case integer_e:
		ret = Py_BuildValue("L", arg.integer); // TODO PyLong_FromLongLong instad?
		break;
	case floating_e:
		ret = Py_BuildValue("d", arg.floating);
		break;
	case string_e:
		assert(arg.string.buf);
		ret = PyUnicode_FromStringAndSize(arg.string.buf, arg.string.size);
		free(arg.string.buf); // malloc in elisp2c->emacs_value_string2argument
		break;
	case boolean_e:
		ret = (arg.boolean ? Py_True : Py_False);
		break;
	case cons_e:
		// TODO check if len fits in Py_ssize_t!!
		assert(arg.list.buf);
		ret = PyList_New(arg.list.size);
		for (size_t i = 0; i < arg.list.size; ++i) {
			PyList_SET_ITEM(ret, i,
			    c2py(arg.list.buf[i])); // TODO ensure its not == NULL)
		}
		free(arg.list.buf); // malloc in elisp2c
		break;
	case hash_e:
		ret = PyDict_New(); // TODO it needs decref if function is just freeing memory
				    // during error handling
		assert(arg.hash.keys.size == arg.hash.values.size);
		for (size_t i = 0; i < arg.hash.keys.size;
		     ++i) { // elisp2c dont allocate memory for empty hash
			k = c2py(arg.hash.keys.buf[i]);
			v = c2py(arg.hash.values.buf[i]);
			if (k && v) {
				PyDict_SetItem(ret, k, v);
			}
			Py_XDECREF(k);
			Py_XDECREF(
			    v); // "PyDict_SetItem `does not' steal a reference to `val'."
		}
		break;
	case exception_e:
		PyErr_Format(PyExc_TypeError,
		    "Conversion of error from elisp to python is not supported");
		break;
	case symbol_e:
		PyErr_Format(PyExc_TypeError,
		    "Conversion of symbol from elisp to python is not supported yet");
		break;
	default:
		PyErr_Format(PyExc_TypeError, "Unable to convert");
		break;
	}
	return ret;
}

PyObject **convert_args_c2py(struct python_call call, ptrdiff_t wanted_nargs,
    argument_type *arg_types) {
	struct argument *cargs = call.args;
	ptrdiff_t nargs = call.nargs;

	if (nargs != wanted_nargs) {
		PyErr_Format(PyExc_TypeError,
		    "Wrong number of arguments (had '%zd' vs '%zd' wanted).", // TODO is this
									      // format ok??
		    nargs, wanted_nargs);
	}

	PyObject **pyargs = malloc(nargs * sizeof(PyObject *)); // free in caller
	for (ptrdiff_t i = 0; i < nargs; ++i) {
		if (PyErr_Occurred()) {
			free(pyargs);
			pyargs = NULL;
			break; // TODO now its memory leak of unconverted value
		}
		if (cargs[i].type == arg_types[i])
			pyargs[i] = c2py(cargs[i]);
		else if (cargs[i].type == boolean_e && cargs[i].boolean == false &&
		    arg_types[i] == cons_e)
			pyargs[i] = PyList_New(0);
		else if (cargs[i].type == boolean_e && cargs[i].boolean == false &&
		    arg_types[i] == hash_e)
			pyargs[i] = PyDict_New();
		else {
			free(pyargs);
			pyargs = NULL;
			PyErr_Format(PyExc_TypeError, "Wrong type of argument number '%d'",
			    i + 1);
			break; // TODO now its memory leak of unconverted value
		}

		if (PyErr_Occurred()) {
			free(pyargs);
			pyargs = NULL;
			break;
		}
	}
	free(cargs); // malloc in call_function
	return pyargs;
}

typedef struct subintr_switch_data {
	struct interpr *sub_interpreter;
	PyGILState_STATE gil;
	PyThreadState *orig_tstate;
} subintr_switch_data;

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

char *get_subinterpreter_name_from_call_data(python_call cargs) {
	// todo error handling
	char *subinterpreter_name = cargs.args[0].string.buf;
	assert(cargs.args[0].type == string_e);
	assert(subinterpreter_name);
	return subinterpreter_name;
}

subintr_switch_data subinterpreter_switch(python_call cargs) {
	char *interpreter_name = get_subinterpreter_name_from_call_data(cargs);
	subintr_switch_data ret;
	ret.sub_interpreter = get_interpreter(interpreter_name);
	ret.gil = PyGILState_Ensure();
	// TEMPORARY WORKADOUND. we dont switch subinterpreter for now.
	// uncomment all lines once github.com/813gan/emacs-python-ffi/issues/1 is done
	/* ret.orig_tstate = NULL; */
	/* if (NULL == */
	/*     ret.sub_interpreter) { // TODO dont use python expception outsude subinterpreter
	 */
	/*	PyErr_Format(PyExc_KeyError, "Subinterpreter '%s' does not exist.", */
	/*	    interpreter_name); */
	/*	PyGILState_Release(ret.gil); */
	/*	return ret; */
	/* } */
	/* ret.orig_tstate = PyThreadState_Get(); */
	/* PyThreadState_Swap(ret.sub_interpreter->python_interpreter); */

	return ret;
}

struct argument subinterpreter_return(PyObject *ret, subintr_switch_data switch_data) {
	struct argument cret = py2c(ret);
	(void)(switch_data);
	// TEMPORARY WORKADOUND. we dont switch subinterpreter for now.
	// uncomment all lines below github.com/813gan/emacs-python-ffi/issues/1 is done
	/* if (switch_data.orig_tstate) */
	/*	PyThreadState_Swap(switch_data.orig_tstate); */
	if (PyGILState_Check())
		PyGILState_Release(switch_data.gil);
	return cret;
}

argument make_interpreter(python_call cargs) {
	char *interpreter_name = get_subinterpreter_name_from_call_data(cargs);
	PyGILState_STATE gil = PyGILState_Ensure();
	// TEMPORARY WORKADOUND. we dont switch subinterpreter for now.
	// uncomment all lines below github.com/813gan/emacs-python-ffi/issues/1 is done
	/* PyThreadState *tstate = NULL; */
	/* PyThreadState *orig_tstate = PyThreadState_Get(); */
	argument ret;
	ret.type = boolean_e;

	/* struct interpr *maybe_existing_interpreter = get_interpreter(interpreter_name); */
	/* if (NULL != maybe_existing_interpreter) { */
	/*	PyGILState_Release(gil); */
	/*	ret.boolean = true; */
	/*	return ret; */
	/* } */

	/* tstate = Py_NewInterpreter(); */
	/* if (NULL == tstate) { */
	/*	PyGILState_Release(gil); */
	/*	ret.boolean = false; */
	/*	return ret; */
	/* } */
	/* PyThreadState_Swap(tstate); */

	unsigned int name_len = strnlen(interpreter_name, MAX_INTERPRETER_NAME_LEN) + 1;
	char *name = malloc(name_len);
	assert(name);
	strncpy(name, interpreter_name, name_len);

	PyObject *main_module = Py_NewRef(PyImport_AddModule("__main__"));
	assert(main_module);
	/* PyThreadState_Swap(orig_tstate); */
	PyGILState_Release(gil);

	struct interpr *new_interpreter = malloc(sizeof(struct interpr));
	/* assert(new_interpreter); */
	new_interpreter->name = name;
	/* new_interpreter->python_interpreter = tstate; */
	new_interpreter->main_module = main_module;
	LIST_INSERT_HEAD(&head, new_interpreter, entries);

	ret.boolean = true;
	return ret;
}

argument destroy_subinterpreter(python_call cargs) {
	subintr_switch_data sw = subinterpreter_switch(cargs);
	argument ret;
	if (PyErr_Occurred())
		goto finish;
	ret.type = boolean_e;
	if (PyErr_Occurred()) {
		ret.boolean = false;
		goto finish;
	}
	ret.boolean = true;
	Py_EndInterpreter(sw.sub_interpreter->python_interpreter);
	free(sw.sub_interpreter->name);
	LIST_REMOVE(sw.sub_interpreter, entries);
	free(sw.sub_interpreter);
finish:
	if (sw.orig_tstate)
		PyThreadState_Swap(sw.orig_tstate);
	if (PyGILState_Check())
		PyGILState_Release(sw.gil);
	return ret;
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

argument import_module(python_call cargs) {
	subintr_switch_data sw = subinterpreter_switch(cargs);
	PyObject *module = NULL;
	PyObject *ret = Py_True;
	argument_type arg_types[] = { string_e, string_e, string_e };
	PyObject **pyargs;
	PyObject *name;
	PyObject *as;
	PyObject *global_dict;
	if (PyErr_Occurred()) {
		goto finish;
	}
	pyargs = convert_args_c2py(cargs, 3, arg_types);
	if (PyErr_Occurred())
		goto finish;
	name = pyargs[1];
	as = pyargs[2];
	free(pyargs);

	global_dict = PyModule_GetDict(sw.sub_interpreter->main_module);

	module = PyImport_Import(name); // New reference
	if (PyErr_Occurred())
		goto finish;

	PyObject_SetItem(global_dict, as, module);
finish:
	Py_XDECREF(module);
	return subinterpreter_return(ret, sw);
}

argument eval_string(python_call cargs) {
	subintr_switch_data sw = subinterpreter_switch(cargs);
	PyObject *ret = NULL;
	PyObject *obj;
	PyObject *target_name;
	argument_type arg_types[] = { string_e, string_e, string_e };
	PyObject **pyargs;
	PyObject *global_dict;
	PyObject *builtins_name;
	PyObject *method_name;
	PyObject *builtins;
	PyObject *method;
	PyObject *string;
	PyObject *string_pytuple;

	if (PyErr_Occurred())
		goto finish;

	pyargs = convert_args_c2py(cargs, 3, arg_types);
	if (PyErr_Occurred())
		goto finish;
	string = pyargs[1];
	target_name = pyargs[2];
	free(pyargs);

	global_dict = PyModule_GetDict(sw.sub_interpreter->main_module);
	builtins_name = PyUnicode_FromString("__builtins__");	 // New reference
	method_name = PyUnicode_FromString("eval");		 // New reference
	builtins = PyObject_GetItem(global_dict, builtins_name); // New reference
	Py_DECREF(builtins_name);
	method = PyObject_GetAttr(builtins, method_name); // New reference
	Py_DECREF(builtins);
	Py_DECREF(method_name);

	string_pytuple = Py_BuildValue("(OO)", string, global_dict);
	Py_DECREF(string);
	// PyRun_SimpleString takes char* isntead unicode
	obj = PyObject_Call(method, string_pytuple, NULL);
	Py_DECREF(method);
	Py_DECREF(string_pytuple);

	if (PyErr_Occurred())
		goto finish;

	if (PyUnicode_GetLength(target_name) > 0) {
		ret = Py_True;
		PyObject_SetItem(global_dict, target_name, obj);
		Py_DECREF(obj);
	} else {
		ret = obj;
	}
finish:
	return subinterpreter_return(ret, sw);
}

argument exec_string(python_call cargs) {
	PyObject *ret = NULL;
	argument_type arg_types[] = { string_e, string_e };
	PyObject **pyargs;
	subintr_switch_data sw = subinterpreter_switch(cargs);
	PyObject *global_dict;
	PyObject *builtins_name;
	PyObject *method_name;
	PyObject *builtins;
	PyObject *method;
	PyObject *string;
	PyObject *string_pytuple;
	if (PyErr_Occurred())
		goto finish;

	pyargs = convert_args_c2py(cargs, 2, arg_types);
	if (PyErr_Occurred())
		goto finish;
	string = pyargs[1];

	free(pyargs);
	ret = Py_True;

	global_dict = PyModule_GetDict(sw.sub_interpreter->main_module);
	builtins_name = PyUnicode_FromString("__builtins__");	 // New reference
	method_name = PyUnicode_FromString("exec");		 // New reference
	builtins = PyObject_GetItem(global_dict, builtins_name); // New reference
	Py_DECREF(builtins_name);
	method = PyObject_GetAttr(builtins, method_name); // New reference
	Py_DECREF(builtins);
	Py_DECREF(method_name);

	string_pytuple = Py_BuildValue("(OO)", string, global_dict);
	Py_DECREF(string);
	// PyRun_SimpleString takes char* isntead unicode
	PyObject_Call(method, string_pytuple, NULL);
	Py_DECREF(method);
	Py_DECREF(string_pytuple);
	// TODO https://docs.python.org/3/c-api/init_config.html#c.PyConfig.inspect
finish:
	return subinterpreter_return(ret, sw);
}

argument call_py(python_call cargs) {
	subintr_switch_data sw = subinterpreter_switch(cargs);
	PyObject *global_dict = NULL;
	PyObject *call_obj = NULL;
	PyObject *obj = NULL;
	PyObject *method = NULL;
	PyObject **pyargs;
	PyObject *obj_name;
	PyObject *ret = NULL;
	PyObject *method_name;
	PyObject *target_name;
	PyObject *args_pytuple;
	PyObject *kwargs_pydict;
	argument_type arg_types[] = { string_e, string_e, string_e, string_e, cons_e, hash_e };

	if (PyErr_Occurred())
		goto finish;
	pyargs = convert_args_c2py(cargs, 6, arg_types);
	if (PyErr_Occurred())
		goto finish;
	obj_name = pyargs[1];
	method_name = pyargs[2];
	target_name = pyargs[3];
	args_pytuple = PyList_AsTuple(pyargs[4]);
	Py_DECREF(pyargs[4]);
	kwargs_pydict = pyargs[5]; // TODO missing DECref?
	free(pyargs);

	assert(args_pytuple);
	global_dict = PyModule_GetDict(sw.sub_interpreter->main_module);
	call_obj = PyObject_GetItem(global_dict, obj_name); // New reference
	assert(global_dict);

	if (NULL == call_obj) {
		PyErr_Clear();
		PyObject *builtins_name = PyUnicode_FromString("__builtins__");
		PyObject *builtins = PyObject_GetItem(global_dict,
		    builtins_name); // New reference
		Py_DECREF(builtins_name);
		assert(obj_name);
		assert(builtins);
		call_obj = PyObject_GetAttr(builtins, obj_name); // New reference
		Py_DECREF(builtins);
	}

	if (PyErr_Occurred())
		goto finish;
	assert(call_obj);

	if (1 != PyObject_IsTrue(kwargs_pydict)) {
		kwargs_pydict = NULL;
	}

	if (1 == PyObject_IsTrue(method_name)) {
		method = PyObject_GetAttr(call_obj, method_name); // New reference

		if (PyErr_Occurred())
			goto finish;

		if (0 == PyCallable_Check(method)) {
			PyErr_SetObject(PyExc_ValueError, method_name);
			goto finish;
		}
		obj = PyObject_Call(method, args_pytuple, kwargs_pydict); // New reference
	} else {
		obj = PyObject_Call(call_obj, args_pytuple, kwargs_pydict); // New reference
	}

	if (obj && PyUnicode_GetLength(target_name) > 0) {
		ret = Py_True;
		PyObject_SetItem(global_dict, target_name, obj);
		Py_DECREF(obj);
	} else {
		ret = obj;
	}
finish:
	Py_XDECREF(call_obj);
	Py_XDECREF(method);
	return subinterpreter_return(ret, sw);
}
