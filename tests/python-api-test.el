(emacspy-setup-subinterpreter "test" (concat emacspy-module-dir "tests"))

(ert-deftest ert-test-python-api-variable ()
  (should (emacspy-import-py "test"
			     (from "python_api_test" import "variable_test") ))
  (should (string= "test" (emacspy-call "test" variable_test))) )

(ert-deftest ert-test-python-api-function ()
  (should (emacspy-import-py "test"
			     (from "python_api_test" import "function_test") ))
  (should (eq 4 (emacspy-call "test" function_test))) )
