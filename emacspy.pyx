# cython: language_level=3str
 
cdef extern from "emacs-module.h":
    ctypedef int emacs_funcall_exit

    ctypedef bint bool

    ctypedef long long int64_t
    ctypedef unsigned long long uint64_t
    ctypedef int64_t intmax_t
    ctypedef uint64_t uintmax_t

    struct emacs_runtime:
        emacs_env* (*get_environment)(emacs_runtime* ert)

    ctypedef struct emacs_value_tag:
        pass

    ctypedef emacs_value_tag* emacs_value

    ctypedef struct emacs_env:
        emacs_value (*funcall) (emacs_env *env,
                                emacs_value function,
                                ptrdiff_t nargs,
                                emacs_value* args) nogil

        emacs_value (*make_function) (emacs_env *env,
                                      ptrdiff_t min_arity,
                                      ptrdiff_t max_arity,
                                      emacs_value (*function) (emacs_env *env,
                                                               ptrdiff_t nargs,
                                                               emacs_value* args,
                                                               void *),
                                      char *documentation,
                                      void *data)

        emacs_value (*intern) (emacs_env *env,
                               const char *symbol_name)

        emacs_value (*type_of) (emacs_env *env,
                                emacs_value value)

        bool (*is_not_nil) (emacs_env *env, emacs_value value)

        bool (*eq) (emacs_env *env, emacs_value a, emacs_value b)
        intmax_t (*extract_integer) (emacs_env *env, emacs_value value)

        emacs_value (*make_integer) (emacs_env *env, intmax_t value)

        double (*extract_float) (emacs_env *env, emacs_value value)

        emacs_value (*make_float) (emacs_env *env, double value)

        emacs_value (*make_string) (emacs_env *env,
                                    const char *contents, ptrdiff_t length)

        bool (*copy_string_contents) (emacs_env *env,
                                      emacs_value value,
                                      char *buffer,
                                      ptrdiff_t *size_inout)

        emacs_value (*make_user_ptr) (emacs_env *env,
                                      void (*fin) (void *),
                                      void *ptr)

        void *(*get_user_ptr) (emacs_env *env, emacs_value uptr)
        void (*set_user_ptr) (emacs_env *env, emacs_value uptr, void *ptr)

        void (*set_user_finalizer) (emacs_env *env,
                                    emacs_value uptr,
                                    void (*fin) (void *))

        emacs_value (*vec_get) (emacs_env *env, emacs_value vec, ptrdiff_t i)

        void (*vec_set) (emacs_env *env, emacs_value vec, ptrdiff_t i,
                         emacs_value val)

        ptrdiff_t (*vec_size) (emacs_env *env, emacs_value vec)

        emacs_value (*make_global_ref) (emacs_env *env,
                                        emacs_value any_reference)

        void (*free_global_ref) (emacs_env *env,
                                 emacs_value global_reference)

        void (*non_local_exit_clear) (emacs_env *env)

        void (*non_local_exit_signal) (emacs_env *env,
                                       emacs_value non_local_exit_symbol,
                                       emacs_value non_local_exit_data)

        void (*non_local_exit_throw) (emacs_env *env,
                                      emacs_value tag,
                                      emacs_value value)

        emacs_funcall_exit (*non_local_exit_get) (emacs_env *env,
                                                  emacs_value *non_local_exit_symbol_out,
                                                  emacs_value *non_local_exit_data_out)


from cython.view cimport array as cvarray
import traceback

cdef extern from "thread_local.c":
    emacs_env* current_env
#cdef emacs_env* current_env = NULL

_defined_functions = []
_dealloc_queue = []

cdef extern from "stdlib.h":
    void abort()
    void* malloc(size_t s)

cdef emacs_env* get_env() except *:
    if current_env == NULL:
        raise Exception('not running in Emacs context, use emacspy_threads.run_in_main_thread')
    return current_env

cdef class _ForDealloc:
    cdef emacs_value v

cdef class EmacsValue:
    cdef emacs_value v

    @staticmethod
    cdef EmacsValue wrap(emacs_value v):
        wrapper = EmacsValue()
        cdef emacs_env* env = get_env()
        wrapper.v = env.make_global_ref(env, v)
        return wrapper

    def __dealloc__(self):
        if current_env == NULL:
            for_dealloc = _ForDealloc()
            for_dealloc.v = self.v
            _dealloc_queue.append(for_dealloc)
        else:
            current_env.free_global_ref(current_env, self.v)
            self.v = NULL

    cpdef str(self):
        cdef emacs_env* env = get_env()
        cdef ptrdiff_t size = -1
        env.copy_string_contents(env, self.v, NULL, &size)
        cdef char* buf = <char*>malloc(size)
        if not env.copy_string_contents(env, self.v, buf, &size):
            raise TypeError('value is not a string')
        assert size > 0
        return buf[:size - 1].decode('utf8')

    cpdef int int(self) except *:
        cdef emacs_env* env = get_env()
        cdef intmax_t i = env.extract_integer(env, self.v)
        return i

    def sym_str(self):
        return _F().symbol_name(self).str()

    def type(self):
        cdef emacs_env* env = get_env()
        return EmacsValue.wrap(env.type_of(env, self.v)).sym_str()

    def to_python_type(self):
        my_type = self.type()
        if my_type == "string":
            return self.str()
        elif my_type == "intiger":
            return self.int()

    def __str__(self):
        return _F().prin1_to_string(self).str()

cdef emacs_value unwrap(obj) except *:
    if isinstance(obj, str):
        obj = string(obj)
    elif obj is True or obj is False: # test for bool must be before int
        obj = make_bool(obj)
    elif isinstance(obj, int):
        obj = make_int(obj)
    elif obj is None:
        obj = nil

    if isinstance(obj, EmacsValue):
        return (<EmacsValue>obj).v
    else:
        raise TypeError("cannot convert %s to emacs value" % type(obj))

cpdef sym(str s):
    cdef emacs_env* env = get_env()
    return EmacsValue.wrap(env.intern(env, s.encode('utf8')))

cdef emacs_value sym_ptr(str s):
    cdef emacs_env* env = get_env()
    return env.intern(env, s.encode('utf8'))

cpdef string(str s):
    cdef emacs_env* env = get_env()
    s_utf8 = s.encode('utf8')
    return EmacsValue.wrap(env.make_string(env, s_utf8, len(s_utf8)))

cpdef make_int(int i):
    cdef emacs_env* env = get_env()
    return EmacsValue.wrap(env.make_integer(env, i))

cpdef make_bool(bool b):
    cdef emacs_env* env = get_env()
    if b:
        return EmacsValue.wrap(env.intern(env, "t".encode('utf8')))
    return nil

cdef emacs_value string_ptr(str s):
    cdef emacs_env* env = get_env()
    s_utf8 = s.encode('utf8')
    return env.make_string(env, s_utf8, len(s_utf8))

class EmacsError(Exception):
    def __init__(self, symbol, data):
        self.symbol = symbol
        self.data = data

    def __str__(self):
        return '%s: %s' % (self.symbol, self.data)

cpdef EmacsValue funcall(f, args):
    args = list(args)
    cdef emacs_env* env = get_env()
    cdef cvarray arg_array = cvarray(shape=(max(1, len(args)), ), itemsize=sizeof(emacs_value), format="i")
    cdef emacs_value* arg_ptr = <emacs_value*>arg_array.data

    for i in range(len(args)):
        arg_ptr[i] = unwrap(args[i])

    cdef emacs_value f_val = unwrap(f)
    cdef int n = len(args)

    cdef emacs_value result

    with nogil:
        result = env.funcall(env, f_val, n, arg_ptr)

    cdef emacs_value exit_symbol
    cdef emacs_value exit_data
    cdef int has_err = env.non_local_exit_get(env, &exit_symbol, &exit_data)
    if has_err != 0:
        env.non_local_exit_clear(env)
        raise EmacsError(EmacsValue.wrap(exit_symbol).sym_str(), str(EmacsValue.wrap(exit_data)))

    return EmacsValue.wrap(result)

cdef emacs_value call_python_object(emacs_env *env, ptrdiff_t nargs, emacs_value* args, void * data) noexcept with gil:
    global current_env
    cdef emacs_env* prev_env = current_env
    current_env = env

    obj = <object>(data)
    arg_list = []
    for i in range(nargs):
        arg_list.append(EmacsValue.wrap(args[i]))

    if _dealloc_queue:
        for item in _dealloc_queue:
            current_env.free_global_ref(current_env, (<_ForDealloc>item).v)
        _dealloc_queue[:] = []

    cdef emacs_value c_result
    try:
        result = obj(*arg_list)
        c_result = unwrap(result)
    except BaseException as exc:
        print('Error in Emacs:')
        traceback.print_exc()
        c_result = string_ptr("error")
        msg = type(exc).__name__ + ': ' + str(exc)
        env.non_local_exit_signal(env, sym_ptr('python-exception'), string_ptr(msg))

    current_env = prev_env
    return c_result

cpdef make_function(obj, str docstring=""):
    cdef emacs_env* env = get_env()
    _defined_functions.append(obj)
    return EmacsValue.wrap(env.make_function(env, 0, 99, call_python_object, docstring.encode('utf8'), <void*>obj))

cdef public int plugin_is_GPL_compatible = 0

eval_python_dict = {}

def str_elisp2c(string):
    return str.encode(string.str())

cdef extern from "subinterpreter.c":
    void make_interpreter(char*)
    object run_string(char*, char*)
    object call_method(object, object, object, object, char*)
    object call_function(object, object, char*)
    void import_module(object, object, char*)
    object get_global_variable(object, char*)
    object get_object_attr(char*, object, object, object)
    object set_global(char*, object, object)

def init():
    @defun('py-make-interpreter')
    def py_make_interpreter(interpreter_name):
        make_interpreter(str.encode(interpreter_name.str()))
        return True

    @defun('py-run-string')
    def py_run_string(run, interpreter_name):
        return run_string(str.encode(run.str()), str.encode(interpreter_name.str()))


    @defun('exec-python')
    def exec_python(s):
        s = s.str()
        exec(s, eval_python_dict)

    @defun('eval-python')
    def eval_python(s):
        s = s.str()
        return eval(s, eval_python_dict)

    @defun('py-import')
    def py_import(name, interpreter_name, as_name=''):
        if not as_name:
            as_name = name
        import_module(name.to_python_type(), as_name.to_python_type(), \
                      str_elisp2c(interpreter_name))
        return True

    @defun('py-call-method')
    def call_object_python(interpreter_name, obj_name, method_name, *args):
        args_py = list((arg.to_python_type() for arg in args))
        ret = call_method(obj_name.to_python_type(), method_name.to_python_type(), args_py, \
                          {}, str_elisp2c(interpreter_name))
        if isinstance(ret, BaseException):
            raise ret
        return ret

    @defun('py-call-function')
    def call_function_python(interpreter_name, function_name, *args):
        args_py = tuple((arg.to_python_type() for arg in args))
        ret = call_function(function_name.to_python_type(),  args_py, \
                            str_elisp2c(interpreter_name))
        if isinstance(ret, BaseException):
            raise ret
        return ret


    @defun('py-get-global-variable')
    def get_global_var(interpreter_name, var_name):
        ret = get_global_variable(var_name.to_python_type(), str_elisp2c(interpreter_name))
        if isinstance(ret, BaseException):
            raise ret
        return ret

    @defun('py-get-object-attr')
    def get_obj_attr(interpreter_name, obj_name, attr_name, target_name=''):
        ret = get_object_attr(str_elisp2c(interpreter_name), obj_name.to_python_type(), \
                              attr_name.to_python_type(), target_name)
        if isinstance(ret, BaseException):
            raise ret
        return ret

    @defun('py-set-global')
    def py_set_global(interpreter_name, obj, target_name):
        ret = set_global(str_elisp2c(interpreter_name), obj.to_python_type(), \
                              target_name.to_python_type())
        if isinstance(ret, BaseException):
            raise ret
        return ret

    _F().define_error(sym('python-exception'), "Python error")
    _F().provide(sym('emacspy'))

cdef public int emacs_module_init_py(emacs_runtime* runtime):
    global current_env, nil
    cdef emacs_env* prev_env = current_env

    current_env = runtime.get_environment(runtime)
    nil = _V().nil
    init()
    current_env = prev_env
    return 0

class _F:
    def __getattr__(self, name):
        name = name.replace('_', '-')

        def f(*args):
            return funcall(sym(name), args)

        return f

    def __getitem__(self, name):
        return getattr(self, name)

# for calling Emacs functions
f = _F()

def defun(name, docstring=""):
    def wrapper(f):
        _F().fset(sym(name), make_function(f, docstring=docstring))
        return f

    return wrapper

class _V:
    def __getattr__(self, name):
        name = name.replace('_', '-')
        return f.symbol_value(sym(name))

    def __setattr__(self, name, value):
        name = name.replace('_', '-')
        f.set(sym(name), value)

    def __getitem__(self, name):
        return getattr(self, name)

    def __setitem__(self, name, value):
        setattr(self, name, value)

# for accessing Emacs variables
v = _V()
