(ert-deftest ert-test-emacspy-py-import ()
  (should-error (py-import "test" "NON_EXISTING_MOD")
                :type 'python-exception) )

(ert-deftest ert-test-emacspy-py-run-string ()
  (should (py-run-string "test" "ospath.realpath('/')"))
  (should-error (py-run-string "test" "some bullshit")
                :type 'python-exception)
  (should (py-run-string "test" "ospath.realpath('/')" "run_string_test_var"))
  (should (string= "/" (py-get-global-variable "test" "run_string_test_var")))
  (should (py-run-string "test" "1==1"))
  (should-not (py-run-string "test" "1==2")) )
(ert-deftest ert-test-emacspy-emacspy--exec-string ()
  (should (emacspy--exec-string "test" "True; False;"))
  (should-error (emacspy--exec-string "test" "some bullshit")
                :type 'python-exception)
  (should (emacspy--exec-string "test" "variable_by_exec = 1"))
  (should (eq 1 (py-get-global-variable "test" "variable_by_exec")))
  (should (emacspy--exec-string "test" "from statistics import median; variable_by_exec = median([1,3])"))
  (should (= 2 (py-get-global-variable "test" "variable_by_exec"))) )

(ert-deftest ert-test-emacspy-data-bool ()
  (should (eq 't (py-run-string "test" "True")))
  (should (eq nil (py-run-string "test" "False")))

  (should (eq 't (py-get-global-variable
                  "test" (py-set-global "test" 't "test_bool"))))
  (should (eq nil (py-get-global-variable
                   "test" (py-set-global "test" () "test_bool")))) )

(ert-deftest ert-test-emacspy-py-call-method ()
  (should (string= "/" (emacspy--call "test" "ospath" "realpath" nil '("/") nil)))
  (should (emacspy--call "test" "ospath" "realpath" "call_method_test_var" '("/") nil))
  (should (string= "/" (py-get-global-variable "test" "call_method_test_var" )))
  (should-error (emacspy--call "test" "ospath" "DUMMY_METHOD" nil (emacspy-alist2hash nil))
                :type 'python-exception)
  (should-error (emacspy--call "test" "NON_EXISTING_OBJECT" "DUMMY_METHOD" nil (emacspy-alist2hash nil))
                :type 'python-exception)
  (should-error (emacspy--call "test" "string" "digits" nil nil nil)
                :type 'python-exception) )

(ert-deftest ert-test-emacspy-py-get-global-variable ()
  (should (string= "__main__" (py-get-global-variable  "test" "__name__")))
  (should-error (py-get-global-variable  "test" "NON_EXISTING_VARIABLE")) )

(ert-deftest ert-test-emacspy-emacspy--call-function ()
  (should (eq 3 (emacspy--call "test" "len" nil nil '("123") nil)))
  (should (emacspy--call "test" "len" nil "call_function_test_var" '("123") nil))
  (should (eq 3 (py-get-global-variable  "test" "call_function_test_var")))
  (should-error (emacspy--call "test" "NON-EXISTING-FUNCTION" nil nil '("123") (emacspy-alist2hash nil))
                :type 'python-exception)

  (should (emacspy--call "test" "dict" nil "call_function_kvargs_test_var" nil
                                  (emacspy-alist2hash '(("some_test" . 1) ("test" . "also_test"))) ))
  (let ((ret (py-get-global-variable "test" "call_function_kvargs_test_var")))
    (should (hash-table-p ret))
    (should (eq 1 (gethash "some_test" ret )))
    (should (string= "also_test" (gethash "test" ret))) ) )

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
  (should-not (emacspy--call "test" "syspath" "append" nil (list (concat emacspy-module-dir "tests")) nil))
  (should (py-import "test" "emacspy_test"))
  (should (py-get-object-attr "test" "emacspy_test" "test_obj" "test_obj"))
  (should (string= "test" (emacspy--call "test" "test_obj" "get_string" nil nil nil))) )

(ert-deftest ert-test-emacspy-non-existing-interpreter ()
  (should-error (py-run-string "NON_EXISTING" "True")))

(ert-deftest ert-test-emacspy-duplicate-load ()
  (should-error (progn (load "emacspy_module")
                       (load "emacspy_module"))))

(ert-deftest ert-test-emacspy-create-destroy-subinterpreter ()
  (let ((sub "test_subinterpreter"))
    (should (py-make-interpreter sub))
    (should (py-make-interpreter sub))
    (should (py-destroy-interpreter sub))
    (should-error (py-get-global-variable  sub "__name__")) ))

(ert-deftest ert-test-emacspy-subinterpreter-isolation () ;; Test if we really switch
  (let ((sub "test2"))
    (should (py-make-interpreter sub))
    (should (py-set-global sub 't "testvarisolation"))
    (should-error (py-get-global-variable "test" "testvarisolation"))
    (should (py-destroy-interpreter sub)) ))

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

(ert-deftest ert-test-emacspy-data-hash ()
  (should (functionp 'emacspy--hash-table-to-lists))
  (let ((hash (make-hash-table))
        (nhash (make-hash-table))
        (empty-hash (make-hash-table)))
    (puthash 1 "test" hash)
    (puthash 2 nil hash)
    (puthash "list" '(1) hash)

    (puthash "key" -1.5 nhash)
    (puthash "hash" nhash hash)

    (should (py-set-global "test" empty-hash "test_empty_hash"))

    (should (py-set-global "test" hash "test_hash"))
    (should (py-run-string "test" "test_hash[1]=='test'"))
    (should (py-run-string "test" "test_hash[2]==False"))
    (should (py-run-string "test" "test_hash['list'][0]==1"))
    (should (py-run-string "test" "test_hash['hash']['key']==-1.5"))

    (let ((py-hash (py-get-global-variable "test" "test_hash")))
      (should (hash-table-p py-hash))
      (should (string= "test" (gethash 1 py-hash)))
      (should (eq nil (gethash 2 py-hash)))
      (should (listp (gethash "list" py-hash)))
      (should (eq 1 (nth 0 (gethash "list" py-hash)) ))
      (should (= -1.5 (gethash "key" (gethash "hash" py-hash)))) ) ))
