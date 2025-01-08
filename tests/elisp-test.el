(add-to-list 'load-path ".")
(load "python-ffi")
(python-ffi-setup-subinterpreter "test")
(py-import "test" "string" "string")
(py-import "test" "os.path" "ospath")

(ert-deftest python-ffi-elisp-import ()
  (should-error (python-ffi-import-py "test"
				   (from "sys" import "NON_EXISTING") ))
  (should (python-ffi-import-py "test"
			     (from "zoneinfo" import "ZoneInfo") ))
  (should (python-ffi-eval-string "test" "'ZoneInfo' in globals()")) )

(ert-deftest python-ffi-call ()
  (should (python-ffi-set-variable-global "test" "test_format_str" "{}{}"))
  (should (string= "test1"
	   (python-ffi-call "test"
			 test_format_str.format "te" "st1")))
  (should (python-ffi-set-variable-global "test" "test_format_str2" "{}{key}"))
  (should (string= "test2"
	   (python-ffi-call "test"
			 test_format_str2.format :kwargs '(key "st2") "te"))) )

(ert-deftest python-ffi-eval/exec ()
  (should (python-ffi-exec-string "test" "True"))
  (should (python-ffi-eval-string "test" "True"))
  (should (python-ffi-eval-string "test" "1" :as "elisp_test_eval_ret_test"))
  (should (eq 1 (python-ffi-eval-string "test" "elisp_test_eval_ret_test"))) )


(ert-deftest python-ffi-set-object-attr ()
  (should (python-ffi-exec-string "test" "class C: pass"))
  (should (python-ffi-exec-string "test" "c=C()"))
  (should-not (python-ffi-set-object-attr "test" "c.random_test_value" "v1"))
  (should (string= "v1" (python-ffi-get-object-attr "test" "c.random_test_value")))
  (should-not (python-ffi-set-object-attr "test" "c" "v2" "random_test_value"))
  (should (string= "v2" (python-ffi-get-object-attr "test" "c.random_test_value"))) )
