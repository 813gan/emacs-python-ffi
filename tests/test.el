(ert-deftest ert-test-emacspy-py-import ()
  (should-error (py-import "test" "NON_EXISTING_MOD")
                :type 'python-exception) )

(ert-deftest ert-test-emacspy-py-run-string ()
  (should (py-run-string "test" "ospath.realpath('/')"))
  (should-error (py-run-string "test" "some bullshit")
                :type 'python-exception)
  (should (py-run-string "test" "ospath.realpath('/')" "run_string_test_var"))
  (should (string= "/" (py-get-global-variable "test" "run_string_test_var")))
  )

(ert-deftest ert-test-emacspy-data-bool ()
  (should (eq 't (py-run-string "test" "True")))
  (should (eq nil (py-run-string "test" "False")))

  (should (eq 't (py-get-global-variable
                  "test" (py-set-global "test" 't "test_bool"))))
  (should (eq nil (py-get-global-variable
                   "test" (py-set-global "test" () "test_bool")))) )

(ert-deftest ert-test-emacspy-py-call-method ()
  (should (string= "/" (py-call-method "test" "ospath" "realpath" nil "/")))
  (should (py-call-method "test" "ospath" "realpath" "call_method_test_var" "/"))
  (should (string= "/" (py-get-global-variable "test" "call_method_test_var")))
  (should-error (string= "/" (py-call-method "test" "ospath" "DUMMY_METHOD"))
                :type 'python-exception)
  (should-error (string= "/" (py-call-method "test" "NON_EXISTING_OBJECT" "DUMMY_METHOD"))
                :type 'python-exception) )

(ert-deftest ert-test-emacspy-py-get-global-variable ()
  (should (string= "__main__" (py-get-global-variable  "test" "__name__"))) )

(ert-deftest ert-test-emacspy-py-call-function ()
  (should (eq 3 (py-call-function "test" "len" nil "123")))
  (should (py-call-function "test" "len" "call_function_test_var" "123"))
  (should (eq 3 (py-get-global-variable  "test" "call_function_test_var")))
  (should-error (py-call-function "test" "NON-EXISTING-FUNCTION" nil "123")
                :type 'python-exception) )

(ert-deftest ert-test-emacspy-py-get-object-attr ()
  (should (string= "0123456789" (py-get-object-attr "test" "string" "digits")))

  (should (py-get-object-attr "test" "string" "digits" "test_digs"))
  (should (string= "0123456789" (py-get-global-variable  "test" "test_digs")))

  (should-error (py-get-object-attr "test" "NON_EXISTING_OBJECT" "digits" "test_digs")
                :type 'python-exception)
  (should-error (py-get-object-attr "test" "string" "NON_EXISTING_ATTR")
                :type 'python-exception) )

(ert-deftest ert-test-emacspy-py-set-global ()
  (should (string= "test_str" (py-set-global "test" "test_value" "test_str")))
  (should (string= "test_value" (py-get-global-variable  "test" "test_str"))) )

(ert-deftest ert-test-emacspy-import-custom-module ()
  (should (py-import "test" "sys"))
  (should (py-get-object-attr "test" "sys" "path" "syspath"))
  (should-not (py-call-method "test" "syspath" "append" nil "./tests/"))
  (should (py-import "test" "emacspy_test"))
  (should (py-get-object-attr "test" "emacspy_test" "test_obj" "test_obj"))
  (should (string= "test" (py-call-method "test" "test_obj" "get_string"))) )

(ert-deftest ert-test-emacspy-non-existing-interpreter ()
  (should-error (py-run-string "NON_EXISTING" "True")))

(ert-deftest ert-test-emacspy-duplicate-load ()
  (should-error (progn (load "emacspy")
                       (load "emacspy"))))

(ert-deftest ert-test-emacspy-data-int ()
  (should (eq 1 (py-get-global-variable
                 "test" (py-set-global "test" 1 "test_int"))))
  (should (eq -1 (py-get-global-variable
                  "test" (py-set-global "test" -1 "test_int"))))
  (should (eq 0 (py-get-global-variable
                 "test" (py-set-global "test" 0 "test_int")))) )
