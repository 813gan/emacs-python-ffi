(ert-deftest ert-test-emacspy-load-prepare-interpreter ()
(ert-deftest ert-test-1-emacspy-load-prepare-interpreter ()
  (add-to-list 'load-path ".")
  (load "emacspy")
  (should (py-make-interpreter "test"))
  (should (py-import "os.path" "test" "path")) )

(ert-deftest ert-test-emacspy-py-run-string ()
  (should (py-run-string "path.realpath('/')" "test"))
  )

(ert-deftest ert-test-emacspy-py-call-method ()
  (should (string= "/" (py-call-method "test" "path" "realpath" "/")))
  )

(ert-deftest ert-test-emacspy-py-get-global-variable ()
  (should (string= "__main__" (py-get-global-variable  "test" "__name__")))
  )

(ert-deftest ert-test-emacspy-py-call-function ()
  (should (eq 3 (py-call-function "test" "len" "123")))
  )
