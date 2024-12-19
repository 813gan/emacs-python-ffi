#include <Python.h>
#include <dlfcn.h>
#include <signal.h>
#include <stdbool.h>
#include <threads.h> // https://en.cppreference.com/w/c/thread

#include "datatypes.h"
#include "emacs-module.h"

int plugin_is_GPL_compatible = 1;

int emacs_module_init_py(void *runtime);
void init_interpreter_list(void);
argument make_interpreter();
argument destroy_subinterpreter(python_call cargs);
// argument list_subinterpreters();
argument import_module(python_call cargs);
argument eval_string(python_call cargs);
argument exec_string(python_call cargs);
argument call_py(python_call cargs);

const python_function null_e = 0;
const python_function make_interpreter_e = 1;
// python_function destroy_subinterpreter_e = 2;
const python_function import_module_e = 3;
const python_function eval_string_e = 4;
const python_function exec_string_e = 5;
const python_function call_py_e = 6;

#define SYM(_EMACSPY_SYMBOLNAME) \
	ENV->intern(ENV, _EMACSPY_SYMBOLNAME) // static function instead?

cnd_t ARG_COND;
mtx_t ARG_MUTEX;
mtx_t RET_MUTEX;
thrd_t EMACSPY_THREAD = 0;

struct python_call PYTHON_CALL;
struct python_return PYTHON_RET;

void emacs_signal_error(emacs_env *ENV, const char *symbol_name, char *error_message) {
	ENV->non_local_exit_signal(ENV, SYM(symbol_name),
	    ENV->make_string(ENV, error_message, strlen(error_message)));
}

void xfree(void *ptr) {
	if (ptr) {
		free(ptr);
	}
}

struct argument emacs_value_string2argument(emacs_env *ENV, emacs_value arg) {
	struct argument ret;
	ptrdiff_t len;
	ret.type = string_e;
	assert(ENV->copy_string_contents(ENV, arg, NULL, &len)); // TODO error handling
	char *buf = malloc(len);				 // free in c2py
	assert(buf);
	ENV->copy_string_contents(ENV, arg, buf, &len);
	ret.string.buf = buf;
	assert(len > 0);
	ret.string.size = len - 1; // exclude terminal NUL from len.
	return ret;
}

struct argument elisp2c(emacs_env *ENV, emacs_value arg) {
	emacs_value arg_type = ENV->type_of(ENV, arg);
	struct argument ret;
	if (ENV->eq(ENV, arg_type, SYM("integer"))) {
		// emacs returns intmax_t but python uses long long int
		static_assert(sizeof(intmax_t) == sizeof(long long int));
		ret.type = integer_e;
		ret.integer = ENV->extract_integer(ENV, arg);
	} else if (ENV->eq(ENV, arg_type, SYM("float"))) {
		ret.type = floating_e;
		ret.floating = ENV->extract_float(ENV, arg);
	} else if (ENV->eq(ENV, arg_type, SYM("string"))) {
		// todo ensure byte len fits in PY_SSIZE_T_MAX
		ret = emacs_value_string2argument(ENV, arg);
		assert(ret.type == string_e);
	} else if (ENV->eq(ENV, arg_type, SYM("symbol"))) {
		if (ENV->eq(ENV, arg, SYM("t"))) {
			ret.type = boolean_e;
			ret.boolean = true;
		} else if (ENV->eq(ENV, arg, SYM("nil"))) {
			ret.type = boolean_e;
			ret.boolean = false;
		} else {
			ret.type = symbol_e;
			emacs_value symbol_name = ENV->funcall(ENV, SYM("symbol-name"), 1,
			    &arg);
			struct argument str = emacs_value_string2argument(ENV, symbol_name);
			ret.symbol.size = str.string.size;
			ret.symbol.buf = str.string.buf;
		}
	} else if (ENV->eq(ENV, arg_type, SYM("cons"))) {
		ret.type = cons_e;
		emacs_value cdr = arg;
		emacs_value car;

		if (!ENV->is_not_nil(ENV, ENV->funcall(ENV, SYM("listp"), 1, &cdr))) {
			emacs_signal_error(ENV, "emacspy-conversion-from-elisp-failed",
			    "Attempted list translation on non list");
		}
		ret.list.size = ENV->extract_integer(ENV,
		    ENV->funcall(ENV, SYM("seq-length"), 1, &cdr));
		// todo check for  overflow  from emacs error
		ret.list.buf = malloc(ret.list.size * sizeof(struct argument)); // free in c2py
		for (size_t i = 0; i < ret.list.size; ++i) {
			car = ENV->funcall(ENV, SYM("car"), 1, &cdr);
			ret.list.buf[i] = elisp2c(ENV, car);
			cdr = ENV->funcall(ENV, SYM("cdr"), 1, &cdr);
		}
	} else if (ENV->eq(ENV, arg_type, SYM("hash-table"))) {
		ret.type = hash_e; // test me for empty hash.
		emacs_value kv_list = ENV->funcall(ENV, SYM("emacspy--hash-table-to-lists"), 1,
		    &arg);
		emacs_value emacs_k_list = ENV->funcall(ENV, SYM("car"), 1, &kv_list);
		emacs_value emacs_v_list = ENV->funcall(ENV, SYM("cadr"), 1, &kv_list);

		if (ENV->is_not_nil(ENV, emacs_k_list)) {
			assert(ENV->is_not_nil(ENV, emacs_v_list));
			argument k_list = elisp2c(ENV, emacs_k_list);
			argument v_list = elisp2c(ENV, emacs_v_list);

			ret.hash.keys.size = k_list.list.size;
			ret.hash.keys.buf = k_list.list.buf;
			ret.hash.values.size = v_list.list.size;
			ret.hash.values.buf = v_list.list.buf;
		} else {
			// elisp2c returns symbol nil instead empty list
			ret.hash.keys.size = 0;
			ret.hash.keys.buf = NULL;
			ret.hash.values.size = 0;
			ret.hash.values.buf = NULL;
		}
	} else {
		emacs_signal_error(ENV, "emacspy-conversion-from-elisp-failed",
		    "invalid datatype");
	}
	/* if (emacs_funcall_exit_signal == ENV->non_local_exit_check(ENV)) */
	/*	ret.type = exception_e; */ // TODO error handling
	return ret;
}

emacs_value c2elisp(emacs_env *ENV, struct argument arg) {
	emacs_value ret = NULL;
	emacs_value *args;
	argument hash_keys_list;
	argument hash_values_list;
	argument exception_type_str;
	argument exception_value_str;
	argument exception_tb_str;
	emacs_value hash_args[2];
	emacs_value exception_value_and_tb[5];
	// todo emacs error handling. i must abort but still free memory from c arg
	switch (arg.type) {
	case integer_e:
		ret = ENV->make_integer(ENV, arg.integer);
		break;
	case floating_e:
		ret = ENV->make_float(ENV, arg.floating);
		break;
	case string_e:
		// PY_SSIZE_T_MAX
		ret = ENV->make_string(ENV, arg.string.buf, arg.string.size);
		free(arg.string.buf); // malloc in py2c
		break;
	case cons_e:
		args = malloc(arg.list.size * sizeof(emacs_value));
		for (size_t i = 0; i < arg.list.size; ++i) {
			// Don't abort on error since elisp2c must free memory
			args[i] = c2elisp(ENV, arg.list.buf[i]);
		}
		ret = ENV->funcall(ENV, SYM("list"), arg.list.size, args);
		free(arg.list.buf); // malloc in c2elisp
		free(args);
		break;
	case hash_e:
		hash_keys_list.type = cons_e;
		hash_keys_list.list.size = arg.hash.keys.size;
		hash_keys_list.list.buf = arg.hash.keys.buf;

		hash_values_list.type = cons_e;
		hash_values_list.list.size = arg.hash.values.size;
		hash_values_list.list.buf = arg.hash.values.buf;

		hash_args[0] = c2elisp(ENV, hash_keys_list);
		hash_args[1] = c2elisp(ENV, hash_values_list);

		ret = ENV->funcall(ENV, SYM("emacspy--lists-to-hash-table"), 2, hash_args);
		break;
	case boolean_e:
		ret = (arg.boolean ? SYM("t") : SYM("nil"));
		break;
	case exception_e:
		exception_type_str.type = string_e;
		exception_type_str.string = arg.exception.type;
		exception_value_str.type = string_e;
		exception_value_str.string = arg.exception.value;
		exception_tb_str.type = string_e;
		exception_tb_str.string = arg.exception.traceback;
		exception_value_and_tb[0] = c2elisp(ENV, exception_type_str);
		exception_value_and_tb[1] = ENV->make_string(ENV, "\n", 1);
		exception_value_and_tb[2] = c2elisp(ENV, exception_value_str);
		exception_value_and_tb[3] = ENV->make_string(ENV, "\n", 1);
		exception_value_and_tb[4] = c2elisp(ENV, exception_tb_str);
		ENV->non_local_exit_signal(ENV, SYM("python-exception"),
		    ENV->funcall(ENV, SYM("concat"), 5, exception_value_and_tb));
		ret = SYM("nil");
		break;
	case symbol_e:
		emacs_signal_error(ENV, "emacspy-conversion-from-python-failed",
		    "impossible return value type: symbol");
		ret = SYM("nil");
		break;
		/* default: */
		/*	emacs_signal_error(ENV, "emacspy-conversion-from-python-failed",
		 * "invalid datatype"); */
		/*	break; */
	}
	if (!ret) {
		ret = SYM("you-shouldnt-see-this-symbol--please-file-bug-reaport");
	}
	return ret;
}
void raise_py_init_fail() {
	mtx_lock(&ARG_MUTEX);
	PYTHON_CALL.status = initialisation_fail;
	mtx_unlock(&ARG_MUTEX);
	thrd_exit(0);
}

int python_worker_thread_f(void *data) {
	(void)(data); // Mute unused argument warning
	assert(!Py_IsInitialized());
	dlopen(LIBPYTHON_NAME, RTLD_LAZY | RTLD_GLOBAL);

	PyConfig config;
	PyStatus status;

	PyConfig_InitPythonConfig(&config);
	status = PyConfig_SetString(&config, &config.home, BASE_PREFIX);
	if (PyStatus_Exception(status)) {
		raise_py_init_fail();
		// return 3;
	}
	status = Py_InitializeFromConfig(&config);
	if (PyStatus_Exception(status)) {
		raise_py_init_fail();
		// return 4;
	}

	init_interpreter_list();
	PyEval_SaveThread();

	int wait_status;
	python_function func;
	struct python_return ret;
	struct python_call call;
	while (true) {
		mtx_lock(&ARG_MUTEX); // TODO error handling
		PYTHON_CALL.status = ready;
		wait_status = cnd_wait(&ARG_COND, &ARG_MUTEX);
		assert(thrd_success == wait_status);
		if (null_e == PYTHON_CALL.func) {
			mtx_lock(&RET_MUTEX);
			PYTHON_RET.status = invalid;
			mtx_unlock(&RET_MUTEX);
			continue; // what the fuck is spurious wakeup?
		}
		call = PYTHON_CALL;
		func = PYTHON_CALL.func;
		PYTHON_CALL.func = null_e;
		PYTHON_CALL.args = NULL;
		PYTHON_CALL.status = in_progress;
		mtx_unlock(&ARG_MUTEX);

		if (call_py_e == func) {
			ret.data = call_py(call);
		} else if (eval_string_e == func) {
			ret.data = eval_string(call);
		} else if (exec_string_e == func) {
			ret.data = exec_string(call);
		} else if (import_module_e == func) {
			ret.data = import_module(call);
		} else if (func == make_interpreter_e) {
			ret.data = make_interpreter(call);
		} else {
			printf("malformed call data\n");
			abort();
		}
		ret.status = ok;
		mtx_lock(&RET_MUTEX);
		PYTHON_RET = ret;
		mtx_unlock(&RET_MUTEX);
	}
}

int wait_for_worker() {
	while (true) {
		/* if (env->should_quit (env)) { */ // TODO
		/*	return 5; */
		/* } */
		mtx_lock(&ARG_MUTEX); // todo error handling
		switch (PYTHON_CALL.status) {
		case ready:
			mtx_unlock(&ARG_MUTEX);
			goto finish;
		case initialisation_fail:
			mtx_unlock(&ARG_MUTEX);
			return 4;
		case in_progress:
		case starting:
			mtx_unlock(&ARG_MUTEX);
			thrd_yield(); // add sleep?
			break;
		}
	}
finish:
	return 0;
}

emacs_value call_function(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *selector) {
	assert(0 == wait_for_worker());
	struct argument *cargs = malloc(
	    nargs * sizeof(struct argument)); // free in convert_args_c2py
	for (ptrdiff_t i = 0; i < nargs; ++i) {
		cargs[i] = elisp2c(env, args[i]);
		// abort on NULL==cargs[i]
	}

	mtx_lock(&ARG_MUTEX); // TDOO error handling
	PYTHON_CALL.func = *(python_function *)selector;
	assert(null_e != PYTHON_CALL.func);
	assert(PYTHON_CALL.status == ready);
	PYTHON_CALL.args = cargs;
	PYTHON_CALL.nargs = nargs;
	mtx_unlock(&ARG_MUTEX);
	cnd_signal(&ARG_COND);

	// https://phst.eu/emacs-modules#quitting
	bool waiting = true;
	while (waiting) {
		switch (env->process_input(env)) {
		case emacs_process_input_continue:
			break;
		case emacs_process_input_quit:
			// TODO
			// raise(SIGINT);
			break;
		}

		mtx_lock(&RET_MUTEX); // TDOO error handling
		if (wait != PYTHON_RET.status) {
			waiting = false;
		} else {
			mtx_unlock(&RET_MUTEX);
			thrd_yield(); // add sleep?
		}
	}
	struct argument py_ret_data = PYTHON_RET.data;
	enum python_return_status ret_status = PYTHON_RET.status;
	PYTHON_RET.status = wait;
	mtx_unlock(&RET_MUTEX);

	assert(ret_status != invalid);
	return c2elisp(env,
	    py_ret_data); // TODO if i implement c-g with aborting then there is mem leak ehere
}

void make_funtion(emacs_env *env, const python_function *selector, const char *name,
    ptrdiff_t num_args, const char *docstring) {
	emacs_value func = env->make_function(env, num_args, num_args, call_function,
	    docstring, (void *)selector);
	emacs_value symbol = env->intern(env, name);
	emacs_value args[] = { symbol, func };
	env->funcall(env, env->intern(env, "defalias"), 2, args);
}

int emacs_module_init(struct emacs_runtime *runtime) {
	// https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html#index-emacs_005fmodule_005finit-1
	if ((long unsigned int)runtime->size < sizeof(*runtime)) {
		fprintf(stderr, "%s\n",
		    "ERROR: emacs_module_init: emacspy was compiled for newer version of Emacs.");
		return 1;
	}

	if (EMACSPY_THREAD) {
		fprintf(stderr, "%s\n",
		    "ERROR: emacs_module_init: emacspy thread is already active.");
		return 2;
	}

	emacs_env *env = runtime->get_environment(runtime);
	int s = cnd_init(&ARG_COND);
	assert(thrd_success == s);
	mtx_init(&ARG_MUTEX, mtx_plain);
	mtx_init(&RET_MUTEX, mtx_plain);

	PYTHON_CALL.status = starting;

	int status = thrd_create(&EMACSPY_THREAD, python_worker_thread_f, NULL);
	if (status != 0) {
		return 3;
	}

	emacs_value args[] = { env->intern(env, "emacspy_module") };
	env->funcall(env, env->intern(env, "provide"), 1, args);

	make_funtion(env, &make_interpreter_e, "py-make-interpreter", 1, "");
	make_funtion(env, &import_module_e, "py-import", 3, "");
	make_funtion(env, &eval_string_e, "emacspy--eval-string", 3, "");
	make_funtion(env, &exec_string_e, "emacspy--exec-string", 2, "");
	make_funtion(env, &call_py_e, "emacspy--call", 6, "");

	return wait_for_worker(); // can return 0,4,5 TODO test c-g during wait
}
