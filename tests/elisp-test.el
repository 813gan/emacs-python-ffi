(ert-deftest ert-test-emacspy-elisp-import ()
  (should-error (emacspy-import-py "test"
				   (from "sys" import "NON_EXISTING") ))
  (should (emacspy-import-py "test"
			     (from "zoneinfo" import "ZoneInfo") ))
  (should (py-run-string "test" "'ZoneInfo' in globals()")) )

(ert-deftest ert-test-emacspy-call ()
  (should (emacspy-set-variable-global "test" "test_format_str" "{}{}"))
  (should (string= "test1"
	   (emacspy-call "test"
			 test_format_str.format "te" "st1")))
  (should (emacspy-set-variable-global "test" "test_format_str2" "{}{key}"))
  (should (string= "test2"
	   (emacspy-call "test"
			 test_format_str2.format :kwargs '(key "st2") "te"))) )
