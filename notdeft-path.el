;;; notdeft-path.el --- NotDeft directory dynamic resolution
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; A system for resolving `notdeft-directories' dynamically, based on
;; a configurable `notdeft-path' specification. Might be useful when
;; storing some NotDeft directories on removable filesystems, allowing
;; the command `notdeft-refresh' to be used to update the available
;; `notdeft-directories' list. The function
;; `notdeft-refresh-directories' should be called where necessary to
;; ensure that the list is kept up to date.
;;
;; Suggested use:
;;  (require 'notdeft-path)
;;  (notdeft-refresh-directories)
;;  (add-hook 'notdeft-pre-refresh-hook 'notdeft-refresh-directories)

;;; Code:

(require 'cl-lib)
(require 'notdeft-autoloads)

(defcustom notdeft-path '("~/.deft/")
  "NotDeft directory search path.
A list of strings, or a function returning a list of strings. The
strings should name directories, which may or may not exist."
  :type '(choice
	  (repeat (string :tag "Directory"))
	  (function :tag "Function"))
  :safe (lambda (lst) (cl-every 'stringp lst))
  :group 'notdeft)

(defvar notdeft-directories-changed-hook nil
  "Hook run after each refresh of `notdeft-directories'.
It is called by `notdeft-refresh-directories'.")

(defun notdeft-existing-directories (dirs)
  "Return a list of existing directories DIRS."
  (mapcar 'file-name-as-directory
	  (cl-remove-if-not 'file-directory-p dirs)))

(defun notdeft-resolve-directories ()
  "Resolve directories from `notdeft-path'.
Return the result as a list of strings that are syntactically
directory names, and name existing directories."
  (let ((lst (if (functionp notdeft-path)
		 (funcall notdeft-path)
	       notdeft-path)))
    (unless (listp lst)
      (error "Expected a list: %S" lst))
    (dolist (elem lst)
      (unless (stringp elem)
	(error "Expected a string: %S" elem)))
    (notdeft-existing-directories lst)))

(eval-when-compile
  (defvar notdeft-directories)
  (defvar notdeft-directory))

(defun notdeft-refresh-directories ()
  "Update `notdeft-directories' based on `notdeft-path'.
Perhaps also clear `notdeft-directory', if it is no longer one of
the `notdeft-directories'."
  (setq notdeft-directories (notdeft-resolve-directories))
  (when (and (boundp 'notdeft-directory) notdeft-directory)
    (unless (cl-some (lambda (elem)
		       (file-equal-p notdeft-directory elem))
		     notdeft-directories)
      (setq notdeft-directory nil)))
  (run-hooks 'notdeft-directories-changed-hook)
  notdeft-directories)

(provide 'notdeft-path)

;;; notdeft-path.el ends here
