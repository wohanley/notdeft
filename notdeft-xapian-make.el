;;; notdeft-xapian-make.el --- Xapian backend auto-installer
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by the authors.
;; All rights reserved.
;; Author: MaxSt <max@stoerchle.at>
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; Functionality for compiling the C++ code for the NotDeft Xapian
;; backend, automatically or otherwise.
;;
;; Instead of setting the `notdeft-xapian-program' variable yourself,
;; you may instead load this `notdeft-xapian-make' feature to have the
;; program built and configured automatically. You may additionally
;; need to set the `notdeft-xapian-program-compile-command-format' to
;; something that produces a suitable compiler invocation for your
;; platform.
;;
;; Suggested use:
;;  (autoload 'notdeft-xapian-make-program-when-uncurrent "notdeft-xapian-make")
;;  (add-hook 'notdeft-load-hook 'notdeft-xapian-make-program-when-uncurrent)

;;; Code:

(require 'notdeft-autoloads)

(defcustom notdeft-xapian-program-compile-command-format
  "c++ -o %s %s -std=c++11 -Wall `pkg-config --cflags --libs tclap` `xapian-config --cxxflags --libs`"
  "Compilation shell command.
Can be a `format' string with two \"%s\" directives, or a
function of two arguments, with the first directive or argument
specifying the executable path for the program, and the second
specifying the C++ source file for the notdeft-xapian program. If
it is a function, it should do any necessary shell escaping of
the arguments."
  :type '(choice
	  (string :tag "Format string")
	  (function :tag "Function"))
  :group 'notdeft)

(defcustom notdeft-xapian-program-install-path
  "notdeft-xapian"
  "Path for the notdeft-xapian executable to build.
If the path is not absolute, it is considered relative to
`notdeft-xapian-home'."
  :type 'string
  :safe 'stringp
  :group 'notdeft)

(defvar notdeft-xapian-home
  (expand-file-name "xapian/"
		    (file-name-directory
		     (file-truename (locate-library "notdeft"))))
  "Directory path for notdeft-xapian sources.
Must specify an absolute path.")

(defvar notdeft-xapian-make-buffer-name " *Install notdeft-xapian"
  "Name of the buffer used for compiling notdeft-xapian.")

(defun notdeft-xapian-program-current-p (&optional program)
  "Whether the notdeft-xapian PROGRAM is current.
It is uncurrent if it does not exist as an executable, or if its
source file is newer. PROGRAM defaults to
`notdeft-xapian-program-install-path'."
  (let ((exe-file (expand-file-name
		   (or program notdeft-xapian-program-install-path)
		   notdeft-xapian-home)))
    (when (file-executable-p exe-file)
      (let ((cxx-file (expand-file-name
		       "notdeft-xapian.cc"
		       notdeft-xapian-home)))
	(when (file-exists-p cxx-file)
	  (not (time-less-p
		(nth 5 (file-attributes exe-file))
		(nth 5 (file-attributes cxx-file)))))))))

(defun notdeft-xapian-make-program (&optional program)
  "Compile the notdeft-xapian program.
Use notdeft-xapian sources in `notdeft-xapian-home', and build
the PROGRAM, which defaults to `notdeft-xapian-program-install-path'.
On success, return the path of the built executable."
  (interactive)
  (unless (file-directory-p notdeft-xapian-home)
    (error "Cannot locate notdeft-xapian sources"))
  (let* ((exe-file (expand-file-name
		    (or program notdeft-xapian-program-install-path)
		    notdeft-xapian-home))
	 (cxx-file (expand-file-name
		    "notdeft-xapian.cc"
		    notdeft-xapian-home))
	 (compile-command
	  (if (functionp notdeft-xapian-program-compile-command-format)
	      (funcall notdeft-xapian-program-compile-command-format
		       exe-file cxx-file)
	    (format notdeft-xapian-program-compile-command-format
		    (shell-quote-argument exe-file)
		    (shell-quote-argument cxx-file))))
	 (buffer (get-buffer-create notdeft-xapian-make-buffer-name)))
    (pop-to-buffer notdeft-xapian-make-buffer-name)
    (let ((exit-code
	   (call-process
	    "sh" nil buffer t "-c" compile-command)))
      (unless (zerop exit-code)
	(error "Compilation of notdeft-xapian failed: %s (%d)"
	       compile-command exit-code))
      (unless (file-executable-p exe-file)
	(error (concat "Compilation of notdeft-xapian failed: "
		       "Executable %S not created")
	       exe-file))
      (message "Compilation of notdeft-xapian succeeded: %S" exe-file)
      exe-file)))

(eval-when-compile
  (defvar notdeft-xapian-program))

(defun notdeft-xapian-make-program-when-uncurrent ()
  "Compile notdeft-xapian program when it is uncurrent.
Only do that if the source directory `notdeft-xapian-home'
exists. In that case generate the executable with the target path
of `notdeft-xapian-program-install-path'. Fail quietly if
compilation fails. Set `notdeft-xapian-program' to the program's
absolute path, or to nil if the program does not exist as an
executable and could not be compiled."
  (when notdeft-xapian-program-install-path
    (let ((exe-file (expand-file-name
		     notdeft-xapian-program-install-path
		     notdeft-xapian-home)))
      (setq notdeft-xapian-program
	    (progn
	      (when (and notdeft-xapian-home
			 (file-directory-p notdeft-xapian-home))
		(unless (notdeft-xapian-program-current-p exe-file)
		  (ignore-errors
		    (notdeft-xapian-make-program exe-file))))
	      (when (file-executable-p exe-file)
		exe-file))))))

(provide 'notdeft-xapian-make)

;;; notdeft-xapian-make.el ends here
