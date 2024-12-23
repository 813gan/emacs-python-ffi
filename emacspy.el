;;; emacspy.el --- emacspy enables you to use python modules from Elisp or program Emacs in Python instead of ELisp.       -*- lexical-binding: t; -*-

;; Copyright (C) 2024 zielmicha, 813gan

;; URL: https://github.com/zielmicha/emacspy
;; Author: 813gan
;; Keywords: python
;; Version: 1.0
;; Package: emacspy
;; Package-Requires: ((python-environment))

;;; Commentary:
;; # emacspy.el

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))
(require 'python-environment)

(defvar emacspy-python-name "python3"
  "Name of python executable.")
(defvar emacspy-module-dir (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing emacspy.")

(defun emacspy--hash-table-to-lists (hash)
  "Utility function that convert `HASH' into (list keys values)."
  (let ((keys (hash-table-keys hash))
	(values nil))
    (setq values
	  (mapcar (lambda (key) (gethash key hash)) keys))
    (list keys values) ))

(defun emacspy--lists-to-hash-table (keys values)
  "Utility function that convert lists of `KEYS' and `VALUES' to hash."
  (let ((hash (make-hash-table :test 'equal)))
    (cl-mapcar (lambda (k v) (puthash k v hash))
     keys values)
    hash))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Error-Symbols.html
(define-error 'emacspy-error "Generic emacspy error")
(define-error 'emacspy-error-worker-dead "Python worker thread died." 'emacspy-error)
(define-error 'emacspy-error-worker-init-failed "Python initialization failed." 'emacspy-error)
(define-error 'python-exception "Python exception was raised" 'emacspy-error) ;; TODO emacspy- prefix
(define-error 'emacspy-conversion-from-elisp-failed "emacspy-conversion-from-elisp-failed" 'emacspy-error)
(define-error 'emacspy-conversion-from-python-failed  "emacspy-conversion-from-python-failed" 'emacspy-error)

(require 'emacspy_module)

(defun emacspy--import-py-multipe-objs (subinterpreter module objs)
  (let ((out (list 'progn))
        (temp_mod_symobol (format "_emacspy_import_%s" module)))
    (push `(emacspy-import ,subinterpreter ,module ,temp_mod_symobol) out)
    (dolist (obj objs)
      (push (list 'emacspy-get-object-attr subinterpreter temp_mod_symobol obj :as obj) out) )
    (nreverse out)))

(defun python-environment-packages-paths (&optional root)
  (let* ((ret nil)
	 (dirs (directory-files (python-environment-lib "" root)
				't directory-files-no-dot-files-regexp 't)))
    (dolist (dir dirs ret)
      (push (format "%s/site-packages" dir) ret)) ))

(defun emacspy--import-py-get-import (subinterpreter import-def)
  "Make import sexp from `IMPORT-DEF'.  This is utility function for emacspy-import-py."
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
    (emacspy--import-py-multipe-objs subinterpreter module objs) ))

(defun emacspy--ensure-str (thing)
  (cond
   ((stringp thing) thing)
   ((not thing) "")
   ((symbolp thing) (symbol-name thing)) ))

;; emacspy public API

(defun emacspy-alist2hash (alist)
  "Convert `ALIST' to hash for easy creation of Python dicts."
  (let ((hash (make-hash-table :test 'equal) ))
    (dolist (kv alist)
      (puthash (car kv) (cdr kv) hash))
    hash))

(defun emacspy-kwargs-plist2hash (plist)
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

(defun emacspy-import (subinterpreter module &optional as)
  "Import modules `MODULE' in `SUBINTERPRETER' and optionally bind it as `AS'."
  (if as
      (py-import subinterpreter module as)
    (py-import subinterpreter module module)))

(defun emacspy-python-pip-install (subinterpreter packages &optional virtualenv)
  "Install packages for `SUBINTERPRETER' from string list `PACKAGES'."
    (let* ((packages-shell-arg (mapcar 'shell-quote-argument packages)))
      (python-environment-run-block (append '("pip" "install" "--") packages-shell-arg)
				    subinterpreter virtualenv)))

(defun emacspy-get-base-prefix (subinterpreter)
  "Get base prefix for `SUBINTERPRETER'.
https://docs.python.org/3/library/sys.html#sys.base_prefix"
  (emacspy-import subinterpreter "sys" "__emacspy_sys")
  (emacspy-get-object-attr subinterpreter __emacspy_sys :as "base_prefix"))

(defun emacspy-get-base-prefix-bin (subinterpreter)
  (python-environment-bin "" (emacspy-get-base-prefix subinterpreter)))

(defun emacspy-python-environment-make (subinterpreter &optional packages virtualenv)
  "Create venv for `SUBINTERPRETER' and install modules from string list `PACKAGES'."
  (py-make-interpreter subinterpreter)
  (let ((python-environment-virtualenv
         (list (expand-file-name emacspy-python-name (emacspy-get-base-prefix-bin subinterpreter)) "-m" "venv")))
    (python-environment-make-block subinterpreter virtualenv)
    (when packages
      (emacspy-python-pip-install subinterpreter packages virtualenv) )))

(defun emacspy-env-ready-p (subinterpreter)
  (python-environment-exists-p subinterpreter))

(defun emacspy-setup-subinterpreter (subinterpreter &rest pythonpaths)
  "Create Python subinterpreter called `SUBINTERPRETER' and add strings `PYTHONPATHS' to `sys.path'."
  (py-make-interpreter subinterpreter)
  ;; emacspy-get-variable-global and emacspy-set-variable-global needs `__emacspy_globals'.
  (emacspy--eval-string subinterpreter "globals()" "__emacspy_globals")
  (emacspy-import subinterpreter "sys" "__emacspy_sys")
  (emacspy-get-object-attr subinterpreter "__emacspy_sys" "path" :as "__emacspy_syspath")
  (emacspy--call subinterpreter "__emacspy_syspath" "append" ""
                 (list emacspy-module-dir) (make-hash-table))
  (when (python-environment-exists-p subinterpreter)
    (dolist (path (python-environment-packages-paths subinterpreter))
      (emacspy--call subinterpreter "__emacspy_syspath" "append" "" (list path) (make-hash-table))))
  (dolist (path pythonpaths)
    (emacspy--call subinterpreter "__emacspy_syspath" "append" "" (list path) (make-hash-table))) )

(defmacro emacspy-import-py (subinterpreter &rest import-defs)
  "Execute imports inside `SUBINTERPRETER' according to `IMPORT-DEFS'."
  (cons 'progn (mapcar (apply-partially 'emacspy--import-py-get-import subinterpreter)
                       import-defs)))

(defun emacspy-get-variable-global (subinterpreter name)
  "Get global variable named `NAME' from `SUBINTERPRETER'."
  (let* ((name_str (emacspy--ensure-str name)))
    (emacspy--call subinterpreter "__emacspy_globals" "__getitem__" "" (list name_str) (make-hash-table))))

(defun emacspy-set-variable-global (subinterpreter name value)
  "Set global variable named `NAME' from `SUBINTERPRETER' to `VALUE'. Return `NAME'."
  (let* ((name_str (emacspy--ensure-str name)))
    (emacspy--call subinterpreter "__emacspy_globals" "__setitem__" "" (list name value) (make-hash-table))
    name_str))

(cl-defmacro emacspy-get-object-attr (subinterpreter name &optional field_name &key as &allow-other-keys)
  ""
  (let* ((name_str (emacspy--ensure-str name))
         (name_split (save-match-data (split-string name_str "\\.")))
         (obj_name (nth 0 name_split))
         (field_name (or (nth 1 name_split) field_name)))
    (list 'emacspy--call
          subinterpreter name "__getattribute__" (or as "") (list 'list field_name) (make-hash-table))))

;;; TODO emacspy-set-object-attr

(cl-defmacro emacspy-call (subinterpreter name &rest args &key as kwargs &allow-other-keys)
  ""
  (dolist (key '(:as :kwargs))
    (cl-remf args key))
  (let* ((name_str (emacspy--ensure-str name))
         (name_split (save-match-data (split-string name_str "\\.")))
         (obj_name (nth 0 name_split))
         (method_name (nth 1 name_split))
         (as-str (emacspy--ensure-str as)) )
    `(emacspy--call
      ,subinterpreter ,obj_name ,method_name ,as-str ,(cons 'list args) (emacspy-kwargs-plist2hash ,kwargs))))

(defun emacspy-exec-string (subinterpreter string)
  "Exec (PyRun_SimpleString) `STRING' in `SUBINTERPRETER'.  There is no return value or details about exception."
  (emacspy--exec-string subinterpreter string))

(cl-defun emacspy-eval-string (subinterpreter string &key as)
  "Eval (PyRun_String) `STRING' in `SUBINTERPRETER' optionally binding result to variable named after keyword argument `AS'."
  (emacspy--eval-string subinterpreter string (or as "")))

(provide 'emacspy)

;;; emacspy.el ends here
