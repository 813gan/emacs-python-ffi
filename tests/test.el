(ert-deftest ert-test-1-emacspy-load-prepare-interpreter ()
  (add-to-list 'load-path ".")
  (load "emacspy")
  (should (py-make-interpreter "test"))
  (should (py-import "string" "test"))
  (should (py-import "os.path" "test" "ospath"))
  (should-error (py-import "NON_EXISTING_MOD" "test")
                :type 'python-exception) )

(ert-deftest ert-test-emacspy-py-run-string ()
  (should (py-run-string "ospath.realpath('/')" "test"))
  (should-error (py-run-string "some bullshit" "test")
                :type 'python-exception)
  )

(ert-deftest ert-test-emacspy-data-bool ()
  (should (eq 't (py-run-string "True" "test")))
  (should (eq nil (py-run-string "False" "test"))) )

(ert-deftest ert-test-emacspy-py-call-method ()
  (should (string= "/" (py-call-method "test" "ospath" "realpath" "/")))
  (should-error (string= "/" (py-call-method "test" "ospath" "DUMMY_METHOD"))
                :type 'python-exception)
  (should-error (string= "/" (py-call-method "test" "NON_EXISTING_OBJECT" "DUMMY_METHOD"))
                :type 'python-exception) )

(ert-deftest ert-test-emacspy-py-get-global-variable ()
  (should (string= "__main__" (py-get-global-variable  "test" "__name__")))
  )

(ert-deftest ert-test-emacspy-py-call-function ()
  (should (eq 3 (py-call-function "test" "len" "123")))
  (should-error (py-call-function "test" "NON-EXISTING-FUNCTION" "123")
                :type 'python-exception)
  )

(ert-deftest ert-test-emacspy-py-get-object-attr ()
  (should (string= "0123456789" (py-get-object-attr "test" "string" "digits")))
  )

(ert-deftest ert-test-emacspy-py-set-global ()
  (should (py-set-global "test" "test_str" "test_value"))
  (should (string= "test_value" (py-get-global-variable  "test" "test_str")))
  )
