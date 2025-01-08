;;; python-ffi.el --- Emacs dynamic module that implements foreign function interface for python. It allows usage of python modules in elisp.       -*- lexical-binding: t; -*-

;; Copyright (C) 2024 813gan

;; URL: https://github.com/813gan/emacs-python-ffi
;; Author: 813gan
;; Keywords: python
;; Version: 1.0
;; Package: python-ffi
;; Package-Requires: ((python-environment))

;;; Commentary:
;; See related info entry or online docs at https://813gan.github.io/emacs-python-ffi/

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))
(require 'python-environment)

(defvar python-ffi-python-name "python3"
  "Name of python executable.")
(defvar python-ffi-module-dir (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing python-ffi.")

(defun python-ffi--hash-table-to-lists (hash)
  "Utility function that convert `HASH' into (list keys values)."
  (let ((keys (hash-table-keys hash))
	(values nil))
    (setq values
	  (mapcar (lambda (key) (gethash key hash)) keys))
    (list keys values) ))

(defun python-ffi--lists-to-hash-table (keys values)
  "Utility function that convert lists of `KEYS' and `VALUES' to hash."
  (let ((hash (make-hash-table :test 'equal)))
    (cl-mapcar (lambda (k v) (puthash k v hash))
     keys values)
    hash))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Error-Symbols.html
(define-error 'python-ffi-error "Generic python-ffi error")
(define-error 'python-ffi-error-worker-dead "Python worker thread died." 'python-ffi-error)
(define-error 'python-ffi-error-worker-init-failed "Python initialization failed." 'python-ffi-error)
(define-error 'python-exception "Python exception was raised" 'python-ffi-error) ;; TODO python-ffi- prefix
(define-error 'python-ffi-conversion-from-elisp-failed "python-ffi-conversion-from-elisp-failed" 'python-ffi-error)
(define-error 'python-ffi-conversion-from-python-failed  "python-ffi-conversion-from-python-failed" 'python-ffi-error)

(require 'python-ffi-module)

(defun python-ffi--import-py-multipe-objs (subinterpreter module objs)
  (let ((out (list 'progn))
        (temp_mod_symobol (format "_python_ffi_import_%s" module)))
    (push `(python-ffi-import ,subinterpreter ,module ,temp_mod_symobol) out)
    (dolist (obj objs)
      (push (list 'python-ffi-get-object-attr subinterpreter temp_mod_symobol obj :as obj) out) )
    (nreverse out)))

(defun python-environment-packages-paths (&optional root)
  (let* ((ret nil)
	 (dirs (directory-files (python-environment-lib "" root)
				't directory-files-no-dot-files-regexp 't)))
    (dolist (dir dirs ret)
      (push (format "%s/site-packages" dir) ret)) ))

(defun python-ffi--import-py-get-import (subinterpreter import-def)
  "Make import sexp from `IMPORT-DEF'.  This is utility function for python-ffi-import-py."
  (let ((module) (objs) (as)
        (reading-module) (reading-obj)
        (head) (tail import-def))
    (while tail
      (setq head (car tail)
            tail (cdr tail))
      (cond
       ((eq head 'from)
        (setq reading-module 't) )
       ((eq head 'import)
        (setq reading-obj 't
              reading-module nil))
       ;;;;;;;;;
       (reading-module
        (setq module head
              reading-module nil))
       (reading-obj
        (push head objs)) ))
    (python-ffi--import-py-multipe-objs subinterpreter module objs) ))

(defun python-ffi--ensure-str (thing)
  (cond
   ((stringp thing) thing)
   ((not thing) "")
   ((symbolp thing) (symbol-name thing)) ))

;; python-ffi public API

(defun python-ffi-alist2hash (alist)
  "Convert `ALIST' to hash for easy creation of Python dicts."
  (let ((hash (make-hash-table :test 'equal) ))
    (dolist (kv alist)
      (puthash (car kv) (cdr kv) hash))
    hash))

(defun python-ffi-kwargs-plist2hash (plist)
  "Convert `PLIST' to hash for easy creation of Python dicts.  Plist keys are assumed to be symbols."
  (when plist
    (let ((lcar) (lcadr) (kw-name)
          (lcddr plist)
          (hash (make-hash-table :test 'equal) ))
      (while lcddr
        (setq lcar (car lcddr)
              lcadr (cadr lcddr)
              lcddr (cddr lcddr))
        (setq kw-name (symbol-name lcar))
        (puthash kw-name lcadr hash) )
      hash)))

(defun python-ffi-import (subinterpreter module &optional as)
  "Import modules `MODULE' in `SUBINTERPRETER' and optionally bind it as `AS'."
  (if as
      (py-import subinterpreter module as)
    (py-import subinterpreter module module)))

(defun python-ffi-python-pip-install (subinterpreter packages &optional virtualenv)
  "Install packages for `SUBINTERPRETER' from string list `PACKAGES'."
    (let* ((packages-shell-arg (mapcar 'shell-quote-argument packages)))
      (python-environment-run-block (append '("pip" "install" "--") packages-shell-arg)
				    subinterpreter virtualenv)))

(defun python-ffi-get-base-prefix (subinterpreter)
  "Get base prefix for `SUBINTERPRETER'.
https://docs.python.org/3/library/sys.html#sys.base_prefix"
  (python-ffi-import subinterpreter "sys" "__python_ffi_sys")
  (python-ffi-get-object-attr subinterpreter __python-ffi_sys :as "base_prefix"))

(defun python-ffi-get-base-prefix-bin (subinterpreter)
  (python-environment-bin "" (python-ffi-get-base-prefix subinterpreter)))

(defun python-ffi-python-environment-make (subinterpreter &optional packages virtualenv)
  "Create venv for `SUBINTERPRETER' and install modules from string list `PACKAGES'."
  (py-make-interpreter subinterpreter)
  (let ((python-environment-virtualenv
         (list (expand-file-name python-ffi-python-name (python-ffi-get-base-prefix-bin subinterpreter)) "-m" "venv")))
    (python-environment-make-block subinterpreter virtualenv)
    (when packages
      (python-ffi-python-pip-install subinterpreter packages virtualenv) )))

(defun python-ffi-env-ready-p (subinterpreter)
  (python-environment-exists-p subinterpreter))

(defun python-ffi-setup-subinterpreter (subinterpreter &rest pythonpaths)
  "Create Python subinterpreter called `SUBINTERPRETER' and add strings `PYTHONPATHS' to `sys.path'."
  (py-make-interpreter subinterpreter)
  ;; python-ffi-get-variable-global and python-ffi-set-variable-global needs `__python-ffi_globals'.
  (python-ffi--eval-string subinterpreter "globals()" "__python_ffi_globals")
  (python-ffi-import subinterpreter "sys" "__python_ffi_sys")
  (python-ffi-get-object-attr subinterpreter "__python_ffi_sys" "path" :as "__python_ffi_syspath")
  (python-ffi--call subinterpreter "__python_ffi_syspath" "append" ""
                 (list python-ffi-module-dir) (make-hash-table))
  (when (python-environment-exists-p subinterpreter)
    (dolist (path (python-environment-packages-paths subinterpreter))
      (python-ffi--call subinterpreter "__python_ffi_syspath" "append" "" (list path) (make-hash-table))))
  (dolist (path pythonpaths)
    (python-ffi--call subinterpreter "__python_ffi_syspath" "append" "" (list path) (make-hash-table))) )

(defmacro python-ffi-import-py (subinterpreter &rest import-defs)
  "Execute imports inside `SUBINTERPRETER' according to `IMPORT-DEFS'."
  (cons 'progn (mapcar (apply-partially 'python-ffi--import-py-get-import subinterpreter)
                       import-defs)))

(defun python-ffi-get-variable-global (subinterpreter name)
  "Get global variable named `NAME' from `SUBINTERPRETER'."
  (let* ((name_str (python-ffi--ensure-str name)))
    (python-ffi--call subinterpreter "__python_ffi_globals" "__getitem__" "" (list name_str) (make-hash-table))))

(defun python-ffi-set-variable-global (subinterpreter name value)
  "Set global variable named `NAME' from `SUBINTERPRETER' to `VALUE'. Return `NAME'."
  (let* ((name_str (python-ffi--ensure-str name)))
    (python-ffi--call subinterpreter "__python_ffi_globals" "__setitem__" "" (list name value) (make-hash-table))
    name_str))

(cl-defmacro python-ffi-get-object-attr (subinterpreter name &optional field_name &key as &allow-other-keys)
  ""
  (let* ((name_str (python-ffi--ensure-str name))
         (name_split (save-match-data (split-string name_str "\\.")))
         (obj_name (nth 0 name_split))
         (field (or (nth 1 name_split) field_name)))
    (list 'python-ffi--call
          subinterpreter obj_name "__getattribute__" (or as "") (list 'list field) (make-hash-table))))

(cl-defmacro python-ffi-set-object-attr (subinterpreter name value &optional field_name)
  ""
  (let* ((name_str (python-ffi--ensure-str name))
         (name_split (save-match-data (split-string name_str "\\.")))
         (obj_name (nth 0 name_split))
         (field (or (nth 1 name_split) field_name)))
    (list 'python-ffi--call
          subinterpreter obj_name "__setattr__" "" (list 'list field value) (make-hash-table))))

(cl-defmacro python-ffi-call (subinterpreter name &rest args &key as kwargs &allow-other-keys)
  ""
  (dolist (key '(:as :kwargs))
    (cl-remf args key))
  (let* ((name_str (python-ffi--ensure-str name))
         (name_split (save-match-data (split-string name_str "\\.")))
         (obj_name (nth 0 name_split))
         (method_name (python-ffi--ensure-str (nth 1 name_split)))
         (as-str (python-ffi--ensure-str as)) )
    `(python-ffi--call
      ,subinterpreter ,obj_name ,method_name ,as-str ,(cons 'list args) (python-ffi-kwargs-plist2hash ,kwargs))))

(defun python-ffi-exec-string (subinterpreter string)
  "Exec `STRING' in `SUBINTERPRETER'.  There is no return value or details about exception."
  (python-ffi--exec-string subinterpreter string))

(cl-defun python-ffi-eval-string (subinterpreter string &key as)
  "Eval `STRING' in `SUBINTERPRETER' optionally binding result to variable named after keyword argument `AS'."
  (python-ffi--eval-string subinterpreter string (or as "")))

(provide 'python-ffi)

;;; python-ffi.el ends here
