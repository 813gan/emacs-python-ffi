(require 'cl)

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
  (should (string= "__main__" (py-get-global-variable  "test" "__name__")))
  (should-error (py-get-global-variable  "test" "NON_EXISTING_VARIABLE")) )

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

(ert-deftest ert-test-emacspy-create-destroy-subinterpreter ()
  (let ((sub "test_subinterpreter"))
    (should (py-make-interpreter sub))
    (should (py-make-interpreter sub))
    (should (py-destroy-interpreter sub))
    (should-error (py-get-global-variable  sub "__name__")) ))

(ert-deftest ert-test-emacspy-list-subiterpreters ()
  (let ((ret (py-list-interpreters)))
    (should (listp ret))
    (should (eq 1 (length ret)))
    (should (string= "test" (nth 0 ret))) ))

(ert-deftest ert-test-emacspy-data-int ()
  (should (eq 1 (py-get-global-variable
                 "test" (py-set-global "test" 1 "test_int"))))
  (should (eq -1 (py-get-global-variable
                  "test" (py-set-global "test" -1 "test_int"))))
  (should (eq 0 (py-get-global-variable
                 "test" (py-set-global "test" 0 "test_int")))) )

(ert-deftest ert-test-emacspy-data-float ()
  (should (= 0.5 (py-get-global-variable
                 "test" (py-set-global "test" 0.5 "test_int"))))
  (should (= -0.5 (py-get-global-variable
                    "test" (py-set-global "test" -0.5 "test_int"))))
  (should (= 0.0 (py-get-global-variable
                  "test" (py-set-global "test" 0.0 "test_int")))) )

(ert-deftest ert-test-emacspy-data-str ()
  (should (string= "" (py-run-string "test" "''")))
  (should (string= "str" (py-run-string "test" "'str'")))
  (should (string= "субтитри" (py-run-string "test" "'субтитри'"))) )

(ert-deftest ert-test-emacspy-data-list ()
  (let ((lst (py-run-string "test" "[1, True, 2, 'test']")))
    (should (eq 4 (length lst)))
    (should (eq 1 (nth 0 lst)))
    (should (eq 't (nth 1 lst)))
    (should (eq 2 (nth 2 lst)))
    (should (string= "test" (nth 3 lst))) )

  (let ((lst (py-run-string "test" "(False,)")))
    (should (eq 1 (length lst)))
    (should (eq nil (nth 0 lst))) )

  (let ((lst (py-run-string "test" "([1, 2, 3], 'test', False)")))
    (should (eq 3 (length lst)))
    (let ((nested-lst (nth 0 lst)))
      (should (listp nested-lst))
      (should (eq 3 (length nested-lst)))
      (should (eq 3 (nth 2 nested-lst))) )
    (should (string= "test" (nth 1 lst)))
    (should (eq nil (nth 2 lst))) )

  (let ((lst (py-get-global-variable
                     "test"
                     (py-set-global "test" '(t nil 3 "test") "test_list"))))
    (should (eq 't (nth 0 lst)))
    (should (eq nil (nth 1 lst)))
    (should (eq 3 (nth 2 lst)))
    (should (string= "test" (nth 3 lst)))
    (should (eq 4 (length lst))) )

  (let ((lst (py-get-global-variable
                     "test"
                     (py-set-global "test" '(("test") (1 2 3)) "test_list"))))
    (should (eq 2 (length lst)))
    (let ((nested-lst (nth 0 lst)))
      (should (listp nested-lst))
      (should (eq 1 (length nested-lst)))
      (should (string= "test" (car nested-lst))) )
    (let ((nested-lst (nth 1 lst)))
      (should (listp nested-lst))
      (should (eq 3 (length nested-lst)))
      (should (eq 3 (nth 2 nested-lst))) ) ))
