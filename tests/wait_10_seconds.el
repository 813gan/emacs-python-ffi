(add-to-list 'load-path ".")
(load "emacspy")
(emacspy-setup-subinterpreter "test")

(emacspy-import-py "test"
                   (from "time" import "sleep"))

(message "wait start...")
(emacspy-call "test" "sleep" 10)
(message "wait finished.")
