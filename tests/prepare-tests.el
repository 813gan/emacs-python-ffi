(add-to-list 'load-path ".")
(load "emacspy")
(py-make-interpreter "test")
(py-import "test" "string")
(py-import "test" "os.path" "ospath")