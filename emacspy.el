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
  (py-import subinterpreter "sys")
  (py-get-object-attr subinterpreter "sys" "path" "__emacspy_syspath")
  (when (python-environment-exists-p subinterpreter)
    (dolist (path (python-environment-packages-paths subinterpreter))
      (py-call-method subinterpreter "__emacspy_syspath" "append" nil path)))
  (dolist (path pythonpaths)
    (py-call-method subinterpreter "__emacspy_syspath" "append" nil path)) )

(defun emacspy--import (subinterpreter &rest imports)
  "Import modules from `IMPORTS' in `SUBINTERPRETER'."
  (dolist (import imports)
    (cond
     ((consp import)
      (py-import subinterpreter (car import) (cdr import) ))
     ((stringp import)
      (py-import subinterpreter import)) )))

(provide 'emacspy)

;;; emacspy.el ends here
