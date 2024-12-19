#ifndef DATATYPES_H
#define DATATYPES_H

#include <stddef.h>
#include <stdbool.h>

typedef int python_function;

enum python_return_status {
	wait,
	ok,
	invalid,
};

enum python_call_status {
	ready,
	in_progress,
	starting,
	initialisation_fail,
};

typedef enum argument_type {
	integer_e,
	string_e,
	floating_e,
	symbol_e,
	cons_e,
	hash_e,
	exception_e,
	boolean_e,
} argument_type;

typedef struct py_string {
	char *buf;
	size_t size;
} py_string;

typedef struct py_exception {
	py_string type;
	py_string value;
	py_string traceback;
} py_exception;

// https://stackoverflow.com/q/888386
typedef struct argument argument;
typedef struct py_list py_list;

struct py_list {
	size_t size;
	argument *buf;
};

typedef struct hash_s {
	py_list keys;
	py_list values;
} hash_s;

struct argument {
	argument_type type;
	union {
		long long int integer;
		double floating;
		py_string string;
		py_string symbol;
		py_exception exception;
		bool boolean;
		py_list list;
		hash_s hash;
	};
};


typedef struct python_call {
	python_function func;
	struct argument *args;
	ptrdiff_t nargs;
	enum python_call_status status;
} python_call;

struct python_return {
	enum python_return_status status;
	struct argument data;
};

#endif /* DATATYPES_H */
