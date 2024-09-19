(ert-deftest ert-test-emacspy-elisp-import ()
  (should-error (emacspy-import-py "test"
				   (from "sys" import "NON_EXISTING") ))
  (should (emacspy-import-py "test"
			     (from "zoneinfo" import "ZoneInfo") ))
  (should (emacspy--eval-string "test" "'ZoneInfo' in globals()")) )

(ert-deftest ert-test-emacspy-call ()
  (should (emacspy-set-variable-global "test" "test_format_str" "{}{}"))
  (should (string= "test1"
	   (emacspy-call "test"
			 test_format_str.format "te" "st1")))
  (should (emacspy-set-variable-global "test" "test_format_str2" "{}{key}"))
  (should (string= "test2"
	   (emacspy-call "test"
			 test_format_str2.format :kwargs '(key "st2") "te"))) )

(ert-deftest ert-test-emacspy-eval/exec ()
  (should (emacspy-exec-string "test" "True"))
  (should (emacspy-eval-string "test" "True"))
  (should (emacspy-eval-string "test" "1" :as "elisp_test_eval_ret_test"))
  (should (eq 1 (emacspy-eval-string "test" "elisp_test_eval_ret_test"))) )
