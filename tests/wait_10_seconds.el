(add-to-list 'load-path ".")
(load "python-ffi")
(python-ffi-setup-subinterpreter "test")

(python-ffi-import-py "test"
                   (from "time" import "sleep"))

(message "wait start...")
(python-ffi-call "test" "sleep" 10)
(message "wait finished.")
