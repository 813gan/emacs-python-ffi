(add-to-list 'load-path ".")
(load "python-ffi")
(python-ffi-setup-subinterpreter "test")
(py-import "test" "string" "string")
(py-import "test" "os.path" "ospath")
(py-import "test" "operator" "operator")


(ert-deftest ffi-py-import ()
  (should-error (py-import "test" "NON_EXISTING_MOD" "")
                :type 'python-exception) )

(ert-deftest python-ffi--eval-string ()
  (should (python-ffi--eval-string "test" "ospath.realpath('/')" ""))
  (should-error (python-ffi--eval-string "test" "some bullshit" "")
                :type 'python-exception)
  (should (python-ffi--eval-string "test" "ospath.realpath('/')" "eval_string_test_var"))
  (should (string= "/" (python-ffi-get-variable-global "test" "eval_string_test_var")))
  (should (python-ffi--eval-string "test" "1==1" ""))
  (should-not (python-ffi--eval-string "test" "1==2" "")) )

(ert-deftest python-ffi--exec-string ()
  (should (python-ffi--exec-string "test" "True; False;"))
  (should-error (python-ffi--exec-string "test" "some bullshit")
                :type 'python-exception)
  (should (python-ffi--exec-string "test" "variable_by_exec = 1"))
  (should (eq 1 (python-ffi-get-variable-global "test" "variable_by_exec")))
  (should (python-ffi--exec-string "test" "from statistics import median; variable_by_exec = median([1,3])"))
  (should (= 2 (python-ffi-get-variable-global "test" "variable_by_exec"))) )

(ert-deftest python-ffi-data-bool ()
  (should (eq 't (python-ffi--eval-string "test" "True" "")))
  (should (eq nil (python-ffi--eval-string "test" "False" "")))

  (should (eq 't (python-ffi-get-variable-global
                  "test" (python-ffi-set-variable-global "test" "test_bool" 't))))
  (should (eq nil (python-ffi-get-variable-global
                   "test" (python-ffi-set-variable-global "test" "test_bool" () )))) )

(ert-deftest python-ffi-py-call-method ()
  (should (string= "/" (python-ffi--call "test" "ospath" "realpath" "" '("/") (make-hash-table))))
  (should (python-ffi--call "test" "ospath" "realpath" "call_method_test_var" '("/") (make-hash-table)))
  (should (string= "/" (python-ffi-get-variable-global "test" "call_method_test_var" )))
  (should (eq 2 (python-ffi--call "test" "operator" "add" "" '(1 1) (make-hash-table)) ))
  (should-error (python-ffi--call "test" "ospath" "DUMMY_METHOD" "" nil nil)
                :type 'python-exception)
  (should-error (python-ffi--call "test" "NON_EXISTING_OBJECT" "DUMMY_METHOD" "" nil nil)
                :type 'python-exception)
  (should-error (python-ffi--call "test" "string" "digits" "" nil nil)
                :type 'python-exception) )

(ert-deftest python-ffi-get-variable-global ()
  (should (string= "__main__" (python-ffi-get-variable-global  "test" "__name__")))
  (should-error (python-ffi-get-variable-global  "test" "NON_EXISTING_VARIABLE")) )

(ert-deftest python-ffi--call-function ()
  (should (eq 3 (python-ffi--call "test" "len" "" "" '("123") nil)))
  (should (python-ffi--call "test" "len" "" "call_function_test_var" '("123") nil))
  (should (eq 3 (python-ffi-get-variable-global  "test" "call_function_test_var")))
  (should-error (python-ffi--call "test" "NON-EXISTING-FUNCTION" "" "" '("123") (python-ffi-alist2hash nil))
                :type 'python-exception)

  (should (python-ffi--call "test" "dict" "" "call_function_kvargs_test_var" nil
                                  (python-ffi-alist2hash '(("some_test" . 1) ("test" . "also_test"))) ))
  (let ((ret (python-ffi-get-variable-global "test" "call_function_kvargs_test_var")))
    (should (hash-table-p ret))
    (should (eq 1 (gethash "some_test" ret )))
    (should (string= "also_test" (gethash "test" ret))) ) )

(ert-deftest python-ffi-get-object-attr ()
  (should (string= "0123456789" (python-ffi-get-object-attr "test" "string" "digits")))

  (should (python-ffi-get-object-attr "test" "string" "digits" :as "test_digs"))
  (should (string= "0123456789" (python-ffi-get-variable-global  "test" "test_digs")))

  (should-error (python-ffi-get-object-attr "test" "NON_EXISTING_OBJECT" "digits" :as "test_digs")
                :type 'python-exception)
  (should-error (python-ffi-get-object-attr "test" "string" "NON_EXISTING_ATTR")
                :type 'python-exception) )

(ert-deftest python-ffi-set-variable-global ()
  (should (string= "test_str" (python-ffi-set-variable-global "test" "test_str" "test_value")))
  (should (string= "test_value" (python-ffi-get-variable-global "test" "test_str"))) )

(ert-deftest ffi-import-custom-module ()
  (should (py-import "test" "sys" "sys"))
  (should (python-ffi-get-object-attr "test" "sys" "path" :as "syspath"))
  (should-not (python-ffi--call "test" "syspath" "append" "" (list (concat python-ffi-module-dir "tests")) nil))
  (should (py-import "test" "python_ffi_test" "python_ffi_test"))
  (should (python-ffi-get-object-attr "test" "python_ffi_test" "test_obj" :as "test_obj"))
  (should (string= "test" (python-ffi--call "test" "test_obj" "get_string" "" nil nil))) )

(ert-deftest python-ffi-non-existing-interpreter ()
  (should-error (python-ffi--eval-string "NON_EXISTING" "True")))

(ert-deftest python-ffi-duplicate-load ()
  (should-error (progn (load "python-ffi-module")
                       (load "python-ffi-module"))))

;; (ert-deftest python-ffi-create-destroy-subinterpreter ()
;;   (let ((sub "test_subinterpreter"))
;;     (should (py-make-interpreter sub))
;;     (should (py-make-interpreter sub))
;;     ;(should (py-destroy-interpreter sub))
;;     (should-error (python-ffi-get-variable-global  sub "__name__")) ))

;; TEMPORARY WORKADOUND. we dont switch subinterpreter for now.
;; uncomment all lines below once https://github.com/python/cpython/issues/113130 is fixed
;; (ert-deftest python-ffi-subinterpreter-isolation () ;; Test if we really switch
;;   (let ((sub "test2"))
;;     (python-ffi-setup-subinterpreter sub)
;;     (should (python-ffi-set-variable-global sub "testvarisolation" 't))
;;     (should-error (python-ffi-get-variable-global "test" "testvarisolation"))
;;                                         ;(should (py-destroy-interpreter sub))
;;     ))

;; (ert-deftest python-ffi-list-subiterpreters ()
;;   (let ((ret (py-list-interpreters)))
;;     (should (listp ret))
;;     (should (eq 1 (length ret)))
;;     (should (string= "test" (nth 0 ret))) ))

(ert-deftest python-ffi-data-int ()
  (should (eq 1 (python-ffi-get-variable-global
                 "test" (python-ffi-set-variable-global "test" "test_int" 1))))
  (should (eq -1 (python-ffi-get-variable-global
                  "test" (python-ffi-set-variable-global "test" "test_int" -1))))
  (should (eq 0 (python-ffi-get-variable-global
                 "test" (python-ffi-set-variable-global "test" "test_int" 0)))) )

(ert-deftest python-ffi-data-float ()
  (should (= 0.5 (python-ffi-get-variable-global
                 "test" (python-ffi-set-variable-global "test" "test_int" 0.5))))
  (should (= -0.5 (python-ffi-get-variable-global
                    "test" (python-ffi-set-variable-global "test" "test_int" -0.5))))
  (should (= 0.0 (python-ffi-get-variable-global
                  "test" (python-ffi-set-variable-global "test" "test_int" 0.0)))) )

(ert-deftest python-ffi-data-str ()
  (should (string= "" (python-ffi--eval-string "test" "''" "")))
  (should (string= "str" (python-ffi--eval-string "test" "'str'" "")))
  (should (string= "субтитри" (python-ffi--eval-string "test" "'субтитри'" ""))) )

(ert-deftest python-ffi-data-list ()
  (let ((lst (python-ffi--eval-string "test" "[1, True, 2, 'test']" "")))
    (should (eq 4 (length lst)))
    (should (eq 1 (nth 0 lst)))
    (should (eq 't (nth 1 lst)))
    (should (eq 2 (nth 2 lst)))
    (should (string= "test" (nth 3 lst))) )

  (let ((lst (python-ffi--eval-string "test" "(False,)" "")))
    (should (eq 1 (length lst)))
    (should (eq nil (nth 0 lst))) )

  (let ((lst (python-ffi--eval-string "test" "([1, 2, 3], 'test', False)" "")))
    (should (eq 3 (length lst)))
    (let ((nested-lst (nth 0 lst)))
      (should (listp nested-lst))
      (should (eq 3 (length nested-lst)))
      (should (eq 3 (nth 2 nested-lst))) )
    (should (string= "test" (nth 1 lst)))
    (should (eq nil (nth 2 lst))) )

  (let ((lst (python-ffi-get-variable-global
                     "test"
                     (python-ffi-set-variable-global "test" "test_list" '(t nil 3 "test") ))))
    (should (eq 't (nth 0 lst)))
    (should (eq nil (nth 1 lst)))
    (should (eq 3 (nth 2 lst)))
    (should (string= "test" (nth 3 lst)))
    (should (eq 4 (length lst))) )

  (let ((lst (python-ffi-get-variable-global
                     "test"
                     (python-ffi-set-variable-global "test" "test_list" '(("test") (1 2 3)) ))))
    (should (eq 2 (length lst)))
    (let ((nested-lst (nth 0 lst)))
      (should (listp nested-lst))
      (should (eq 1 (length nested-lst)))
      (should (string= "test" (car nested-lst))) )
    (let ((nested-lst (nth 1 lst)))
      (should (listp nested-lst))
      (should (eq 3 (length nested-lst)))
      (should (eq 3 (nth 2 nested-lst))) ) ))

(ert-deftest python-ffi-data-hash ()
  (should (functionp 'python-ffi--hash-table-to-lists))
  (let ((hash (make-hash-table))
        (nhash (make-hash-table))
        (empty-hash (make-hash-table)))
    (puthash 1 "test" hash)
    (puthash 2 nil hash)
    (puthash "list" '(1) hash)

    (puthash "key" -1.5 nhash)
    (puthash "hash" nhash hash)

    (should (python-ffi-set-variable-global "test" "test_empty_hash" empty-hash))

    (should (python-ffi-set-variable-global "test" "test_hash" hash))
    (should (python-ffi--eval-string "test" "test_hash[1]=='test'" ""))
    (should (python-ffi--eval-string "test" "test_hash[2]==False" ""))
    (should (python-ffi--eval-string "test" "test_hash['list'][0]==1" ""))
    (should (python-ffi--eval-string "test" "test_hash['hash']['key']==-1.5" ""))

    (let ((py-hash (python-ffi-get-variable-global "test" "test_hash")))
      (should (hash-table-p py-hash))
      (should (string= "test" (gethash 1 py-hash)))
      (should (eq nil (gethash 2 py-hash)))
      (should (listp (gethash "list" py-hash)))
      (should (eq 1 (nth 0 (gethash "list" py-hash)) ))
      (should (= -1.5 (gethash "key" (gethash "hash" py-hash)))) ) ))
