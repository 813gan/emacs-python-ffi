;;; emacspy.el --- emacspy enables you to use python modules from Elisp or program Emacs in Python instead of ELisp.       -*- lexical-binding: t; -*-

;; Copyright (C) 2024 zielmicha, 813gan

;; URL: https://github.com/zielmicha/emacspy
;; Author: zielmicha, 813gan
;; Keywords: python
;; Version: 1.0
;; Package: emacspy
;; Package-Requires: ((python-environment))

;;; Commentary:
;; # emacspy.el

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'python-environment)

(defun emacspy--hash-table-to-lists (hash)
  "Utility function that convert `HASH' into (list keys values)."
  (let ((keys (hash-table-keys hash))
	(values nil))
    (setq values
	  (mapcar (lambda (key) (gethash key hash)) keys))
    (list keys values) ))

(require 'emacspy-module)

(defun python-environment-packages-paths (&optional root)
  (let* ((ret nil)
	 (dirs (directory-files (python-environment-lib "" root)
				't directory-files-no-dot-files-regexp 't)))
    (dolist (dir dirs ret)
      (push (format "%s/site-packages" dir) ret)) ))

;; emacspy public API

(defun emacspy-python-pip-install (subinterpreter packages &optional virtualenv)
  "Install packages for `SUBINTERPRETER' from string list `PACKAGES'."
    (let* ((packages-shell-arg (mapcar 'shell-quote-argument packages)))
      (python-environment-run-block (append '("pip" "install" "--") packages-shell-arg)
				    subinterpreter virtualenv)))

(defun emacspy-python-environment-make (subinterpreter &optional packages virtualenv)
  "Create venv for `SUBINTERPRETER' and install modules from string list `PACKAGES'."
  (python-environment-make-block subinterpreter virtualenv)
  (when packages
    (emacspy-python-pip-install subinterpreter packages virtualenv) ))

(defun emacspy-setup-subinterpreter (subinterpreter &rest pythonpaths)
  "Create Python subinterpreter called `SUBINTERPRETER' and add strings `PYTHONPATHS' to `sys.path'."
  (py-make-interpreter subinterpreter)
  (emacspy-import subinterpreter "sys")
  (py-get-object-attr subinterpreter "sys" "path" "__emacspy_syspath")
  (when (python-environment-exists-p subinterpreter)
    (dolist (path (python-environment-packages-paths subinterpreter))
      (emacspy-call-method subinterpreter "__emacspy_syspath" "append" nil path)))
  (dolist (path pythonpaths)
    (emacspy-call-method subinterpreter "__emacspy_syspath" "append" nil path)) )

(defun emacspy--import-py-multipe-objs (subinterpreter module objs)
  (let ((out (list 'progn))
        (temp_mod_symobol (format "_emacspy_import_%s" module)))
    (push `(emacspy-import ,subinterpreter ,module ,temp_mod_symobol) out)
    (dolist (obj objs)
      (push (list 'emacspy-get-object-attr subinterpreter temp_mod_symobol obj obj) out) )
    (nreverse out)))

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

(defmacro emacspy-import-py (subinterpreter &rest import-defs)
  "Execute imports inside `SUBINTERPRETER' according to `IMPORT-DEFS'."
  (cons 'progn (mapcar (apply-partially 'emacspy--import-py-get-import subinterpreter)
                       import-defs)))

(defun emacspy-get-object-attr (subinterpreter obj_name attr_name &optional target_name)
  (py-get-object-attr subinterpreter obj_name attr_name target_name))

(defun emacspy-import (subinterpreter module &optional as)
  "Import modules `MODULE' in `SUBINTERPRETER' and optionally bind it as `AS'."
  (if as
      (py-import subinterpreter module as)
    (py-import subinterpreter module)))

(defun emacspy-call-function (subinterpreter function_name as &rest args)
  (apply 'py-call-function subinterpreter function_name as args))

(defun emacspy-call-method (subinterpreter obj_name method_name as &rest args)
    (apply 'py-call-method subinterpreter obj_name method_name as args))

(provide 'emacspy)

;;; emacspy.el ends here
