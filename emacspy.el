;;; emacspy.el --- emacspy enables you to use python modules from Elisp or program Emacs in Python instead of ELisp.       -*- lexical-binding: t; -*-

;; Copyright (C) 2024 813gan

;; URL: https://github.com/zielmicha/emacspy
;; Author: zielmicha, 813gan
;; Keywords: python
;; Version: 1.0
;; Package: emacspy
;; Package-Requires:

;;; Commentary:
;; # emacspy.el

;;; Code:

(eval-when-compile (require 'subr-x))

(defun emacspy--hash-table-to-lists (hash)
  "Utility function that convert `HASH' into (list keys values)."
  (let ((keys (hash-table-keys hash))
	(values nil))
    (setq values
	  (mapcar (lambda (key) (gethash key hash)) keys))
    (list keys values) ))

(require 'emacspy-module)

;;; emacspy.el ends here
