import emacspy_module as emacspy

def variable_test():
    emacspy.v.emacspy_python_api_test_var1 = "test"
    return emacspy.v.emacspy_python_api_test_var1

def function_test():
    emacspy.f['+'](1, 2)
    return emacspy.f['+'](1, 2).int() + 1
