(add-to-list 'load-path ".")
(load "emacspy")
(emacspy-setup-subinterpreter "test")
(py-import "test" "string" "string")
(py-import "test" "os.path" "ospath")


(ert-deftest ert-test-emacspy-py-import ()
  (should-error (py-import "test" "NON_EXISTING_MOD" "")
                :type 'python-exception) )

(ert-deftest ert-test-emacspy-emacspy--eval-string ()
  (should (emacspy--eval-string "test" "ospath.realpath('/')" ""))
  (should-error (emacspy--eval-string "test" "some bullshit" "")
                :type 'python-exception)
  (should (emacspy--eval-string "test" "ospath.realpath('/')" "eval_string_test_var"))
  (should (string= "/" (emacspy-get-variable-global "test" "eval_string_test_var")))
  (should (emacspy--eval-string "test" "1==1" ""))
  (should-not (emacspy--eval-string "test" "1==2" "")) )

(ert-deftest ert-test-emacspy-emacspy--exec-string ()
  (should (emacspy--exec-string "test" "True; False;"))
  (should-error (emacspy--exec-string "test" "some bullshit")
                :type 'python-exception)
  (should (emacspy--exec-string "test" "variable_by_exec = 1"))
  (should (eq 1 (emacspy-get-variable-global "test" "variable_by_exec")))
  (should (emacspy--exec-string "test" "from statistics import median; variable_by_exec = median([1,3])"))
  (should (= 2 (emacspy-get-variable-global "test" "variable_by_exec"))) )

(ert-deftest ert-test-emacspy-data-bool ()
  (should (eq 't (emacspy--eval-string "test" "True" "")))
  (should (eq nil (emacspy--eval-string "test" "False" "")))

  (should (eq 't (emacspy-get-variable-global
                  "test" (emacspy-set-variable-global "test" "test_bool" 't))))
  (should (eq nil (emacspy-get-variable-global
                   "test" (emacspy-set-variable-global "test" "test_bool" () )))) )

(ert-deftest ert-test-emacspy-py-call-method ()
  (should (string= "/" (emacspy--call "test" "ospath" "realpath" "" '("/") (make-hash-table))))
  (should (emacspy--call "test" "ospath" "realpath" "call_method_test_var" '("/") (make-hash-table)))
  (should (string= "/" (emacspy-get-variable-global "test" "call_method_test_var" )))
  (should-error (emacspy--call "test" "ospath" "DUMMY_METHOD" "" nil nil)
                :type 'python-exception)
  (should-error (emacspy--call "test" "NON_EXISTING_OBJECT" "DUMMY_METHOD" "" nil nil)
                :type 'python-exception)
  (should-error (emacspy--call "test" "string" "digits" "" nil nil)
                :type 'python-exception) )

(ert-deftest ert-test-emacspy-emacspy-get-variable-global ()
  (should (string= "__main__" (emacspy-get-variable-global  "test" "__name__")))
  (should-error (emacspy-get-variable-global  "test" "NON_EXISTING_VARIABLE")) )

(ert-deftest ert-test-emacspy-emacspy--call-function ()
  (should (eq 3 (emacspy--call "test" "len" "" "" '("123") nil)))
  (should (emacspy--call "test" "len" "" "call_function_test_var" '("123") nil))
  (should (eq 3 (emacspy-get-variable-global  "test" "call_function_test_var")))
  (should-error (emacspy--call "test" "NON-EXISTING-FUNCTION" "" "" '("123") (emacspy-alist2hash nil))
                :type 'python-exception)

  (should (emacspy--call "test" "dict" "" "call_function_kvargs_test_var" nil
                                  (emacspy-alist2hash '(("some_test" . 1) ("test" . "also_test"))) ))
  (let ((ret (emacspy-get-variable-global "test" "call_function_kvargs_test_var")))
    (should (hash-table-p ret))
    (should (eq 1 (gethash "some_test" ret )))
    (should (string= "also_test" (gethash "test" ret))) ) )

(ert-deftest ert-test-emacspy-emacspy-get-object-attr ()
  (should (string= "0123456789" (emacspy-get-object-attr "test" "string" "digits")))

  (should (emacspy-get-object-attr "test" "string" "digits" :as "test_digs"))
  (should (string= "0123456789" (emacspy-get-variable-global  "test" "test_digs")))

  (should-error (emacspy-get-object-attr "test" "NON_EXISTING_OBJECT" "digits" :as "test_digs")
                :type 'python-exception)
  (should-error (emacspy-get-object-attr "test" "string" "NON_EXISTING_ATTR")
                :type 'python-exception) )

(ert-deftest ert-test-emacspy-emacspy-set-variable-global ()
  (should (string= "test_str" (emacspy-set-variable-global "test" "test_str" "test_value")))
  (should (string= "test_value" (emacspy-get-variable-global "test" "test_str"))) )

(ert-deftest ert-test-emacspy-import-custom-module ()
  (should (py-import "test" "sys" "sys"))
  (should (emacspy-get-object-attr "test" "sys" "path" :as "syspath"))
  (should-not (emacspy--call "test" "syspath" "append" "" (list (concat emacspy-module-dir "tests")) nil))
  (should (py-import "test" "emacspy_test" "emacspy_test"))
  (should (emacspy-get-object-attr "test" "emacspy_test" "test_obj" :as "test_obj"))
  (should (string= "test" (emacspy--call "test" "test_obj" "get_string" "" nil nil))) )

(ert-deftest ert-test-emacspy-non-existing-interpreter ()
  (should-error (emacspy--eval-string "NON_EXISTING" "True")))

(ert-deftest ert-test-emacspy-duplicate-load ()
  (should-error (progn (load "emacspy_module")
                       (load "emacspy_module"))))

;; (ert-deftest ert-test-emacspy-create-destroy-subinterpreter ()
;;   (let ((sub "test_subinterpreter"))
;;     (should (py-make-interpreter sub))
;;     (should (py-make-interpreter sub))
;;     ;(should (py-destroy-interpreter sub))
;;     (should-error (emacspy-get-variable-global  sub "__name__")) ))

;; TEMPORARY WORKADOUND. we dont switch subinterpreter for now.
;; uncomment all lines below once https://github.com/python/cpython/issues/113130 is fixed
;; (ert-deftest ert-test-emacspy-subinterpreter-isolation () ;; Test if we really switch
;;   (let ((sub "test2"))
;;     (emacspy-setup-subinterpreter sub)
;;     (should (emacspy-set-variable-global sub "testvarisolation" 't))
;;     (should-error (emacspy-get-variable-global "test" "testvarisolation"))
;;                                         ;(should (py-destroy-interpreter sub))
;;     ))

;; (ert-deftest ert-test-emacspy-list-subiterpreters ()
;;   (let ((ret (py-list-interpreters)))
;;     (should (listp ret))
;;     (should (eq 1 (length ret)))
;;     (should (string= "test" (nth 0 ret))) ))

(ert-deftest ert-test-emacspy-data-int ()
  (should (eq 1 (emacspy-get-variable-global
                 "test" (emacspy-set-variable-global "test" "test_int" 1))))
  (should (eq -1 (emacspy-get-variable-global
                  "test" (emacspy-set-variable-global "test" "test_int" -1))))
  (should (eq 0 (emacspy-get-variable-global
                 "test" (emacspy-set-variable-global "test" "test_int" 0)))) )

(ert-deftest ert-test-emacspy-data-float ()
  (should (= 0.5 (emacspy-get-variable-global
                 "test" (emacspy-set-variable-global "test" "test_int" 0.5))))
  (should (= -0.5 (emacspy-get-variable-global
                    "test" (emacspy-set-variable-global "test" "test_int" -0.5))))
  (should (= 0.0 (emacspy-get-variable-global
                  "test" (emacspy-set-variable-global "test" "test_int" 0.0)))) )

(ert-deftest ert-test-emacspy-data-str ()
  (should (string= "" (emacspy--eval-string "test" "''" "")))
  (should (string= "str" (emacspy--eval-string "test" "'str'" "")))
  (should (string= "субтитри" (emacspy--eval-string "test" "'субтитри'" ""))) )

(ert-deftest ert-test-emacspy-data-list ()
  (let ((lst (emacspy--eval-string "test" "[1, True, 2, 'test']" "")))
    (should (eq 4 (length lst)))
    (should (eq 1 (nth 0 lst)))
    (should (eq 't (nth 1 lst)))
    (should (eq 2 (nth 2 lst)))
    (should (string= "test" (nth 3 lst))) )

  (let ((lst (emacspy--eval-string "test" "(False,)" "")))
    (should (eq 1 (length lst)))
    (should (eq nil (nth 0 lst))) )

  (let ((lst (emacspy--eval-string "test" "([1, 2, 3], 'test', False)" "")))
    (should (eq 3 (length lst)))
    (let ((nested-lst (nth 0 lst)))
      (should (listp nested-lst))
      (should (eq 3 (length nested-lst)))
      (should (eq 3 (nth 2 nested-lst))) )
    (should (string= "test" (nth 1 lst)))
    (should (eq nil (nth 2 lst))) )

  (let ((lst (emacspy-get-variable-global
                     "test"
                     (emacspy-set-variable-global "test" "test_list" '(t nil 3 "test") ))))
    (should (eq 't (nth 0 lst)))
    (should (eq nil (nth 1 lst)))
    (should (eq 3 (nth 2 lst)))
    (should (string= "test" (nth 3 lst)))
    (should (eq 4 (length lst))) )

  (let ((lst (emacspy-get-variable-global
                     "test"
                     (emacspy-set-variable-global "test" "test_list" '(("test") (1 2 3)) ))))
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

    (should (emacspy-set-variable-global "test" "test_empty_hash" empty-hash))

    (should (emacspy-set-variable-global "test" "test_hash" hash))
    (should (emacspy--eval-string "test" "test_hash[1]=='test'" ""))
    (should (emacspy--eval-string "test" "test_hash[2]==False" ""))
    (should (emacspy--eval-string "test" "test_hash['list'][0]==1" ""))
    (should (emacspy--eval-string "test" "test_hash['hash']['key']==-1.5" ""))

    (let ((py-hash (emacspy-get-variable-global "test" "test_hash")))
      (should (hash-table-p py-hash))
      (should (string= "test" (gethash 1 py-hash)))
      (should (eq nil (gethash 2 py-hash)))
      (should (listp (gethash "list" py-hash)))
      (should (eq 1 (nth 0 (gethash "list" py-hash)) ))
      (should (= -1.5 (gethash "key" (gethash "hash" py-hash)))) ) ))
