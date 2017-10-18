;;; deft-xapian.el --- Xapian backend for Deft
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "deft.el" for licensing information.

;;; Commentary:
;; Xapian-specific functionality for Deft.

;;; Code:

(defcustom deft-xapian-program nil
  "Xapian backend's executable program path.
Specified as an absolute path.
When nil, incremental search is limited to files
in the current `deft-directory' (if any)."
  :type '(choice (const :tag "None" nil)
		 (file :tag "Path"))
  :safe 'string-or-null-p
  :group 'deft)

(defcustom deft-xapian-max-results 100
  "Maximum number of Xapian query results.
\(I.e., '--max-count' for `deft-xapian-program'.)
No limit if 0."
  :type 'integer
  :safe 'integerp
  :group 'deft)

(defcustom deft-xapian-language "en"
  "Stemming language to use in Xapian indexing and searching."
  :type 'string
  :safe 'stringp
  :group 'deft)

(defcustom deft-xapian-order-by-time t
  "Whether to order file list by decreasing modification time."
  :type 'boolean
  :safe 'booleanp
  :group 'deft)

(defcustom deft-xapian-boolean-any-case t
  "Whether to allow query operators in any case.
That is, whether the operator syntax also allows
lowercase characters (e.g., \"and\" and \"or\")."
  :type 'boolean
  :safe 'booleanp
  :group 'deft)

(defcustom deft-xapian-pure-not t
  "Whether to allow \"NOT\" in queries.
Using such queries is costly on performance."
  :type 'boolean
  :safe 'booleanp
  :group 'deft)

(defface deft-xapian-query-face
  '((t :inherit font-lock-string-face :bold t))
  "Face for Deft Xapian queries."
  :group 'deft-faces)

(defvar deft-xapian-query-history nil
  "Xapian query string history.
Not cleared between invocations of `deft-mode'.")

(defun deft-xapian-read-query ()
  "Read a Xapian query string, interactively.
Use and update `deft-xapian-query-history' in querying.
Return the read string, or nil if no query is given."
  (let ((s (read-from-minibuffer
	    "Query: " ;; PROMPT
	    nil nil nil ;; INITIAL-CONTENTS KEYMAP READ
	    'deft-xapian-query-history ;; HIST
	    nil ;; DEFAULT-VALUE
	    t ;; INHERIT-INPUT-METHOD
	    )))
    (when (and s (not (string= s "")))
      s)))

(eval-when-compile
  (defvar deft-extension)
  (defvar deft-secondary-extensions))

(defun deft-xapian-index-dirs (dirs &optional recreate)
  "Create or update a Xapian index for DIRS.
With RECREATE, truncate any existing index files.
The return value is as for `call-process'."
  (with-temp-buffer
    (let ((ret
	   (apply
	    'call-process
	    deft-xapian-program ;; PROGRAM
	    nil	                ;; INFILE
	    t                   ;; DESTINATION
	    nil	                ;; DISPLAY
	    `("index"
	      "--chdir" ,(expand-file-name "." "~")
	      ,@(if recreate '("--recreate") nil)
	      ,@(apply 'append
		       (mapcar
			(lambda (ext)
			  `("--extension" ,(concat "." ext)))
			(cons deft-extension deft-secondary-extensions)))
	      "--lang" ,deft-xapian-language
	      ,@(mapcar
		 (lambda (dir)
		   (file-relative-name dir "~"))
		 dirs)))))
      (when (/= 0 ret)
	(error "Index generation failed: %s (%d): %s"
	       deft-xapian-program ret (buffer-string))))))

(defun deft-xapian-search (dirs &optional query)
  "On the Xapian indexes in DIRS, perform the search QUERY.
I.e., perform the query in terms of the Xapian indexes
in the specified DIRS. Where a query is not specified,
use a query that matches any file, and in that case
results are ordered by file timestamp regardless of
the value of the variable `deft-xapian-order-by-time'.
Return at most `deft-xapian-max-results' results, as
pathnames of the matching files. Sort the results
based on file modification time, most recent first."
  (let ((time-sort (if query deft-xapian-order-by-time t))
	(max-results deft-xapian-max-results)
	name-sort)
    (when query
      (while (string-match "^ *!\\([[:alpha:]]+\\) +" query)
	(let ((opt (match-string 1 query)))
	  (setq query (substring query (match-end 0)))
	  (pcase (downcase opt)
	    ("time" (setq time-sort t))
	    ("rank" (setq time-sort nil))
	    ("all" (setq max-results 0))
	    ("file" (setq name-sort t))
	    ))))
    (let* ((s (shell-command-to-string
	       (concat
		(shell-quote-argument deft-xapian-program) " search"
		(if time-sort " --time-sort" "")
		" --lang " (shell-quote-argument deft-xapian-language)
		(if deft-xapian-boolean-any-case
		    " --boolean-any-case" "")
		(if deft-xapian-pure-not
		    " --pure-not" "")
		(if (> max-results 0)
		    (format " --max-count %d" deft-xapian-max-results)
		  "")
		(if query
		    (concat " --query " (shell-quote-argument query))
		  "")
		" " (mapconcat
		     (lambda (dir)
		       (shell-quote-argument
			(expand-file-name dir "~")))
		     dirs " "))))
	   (files
	    (mapcar
	     (lambda (file)
	       (expand-file-name file "~"))
	     (split-string s "\n" t))))
      (if name-sort
	  (deft-sort-files-by-name files)
	files))))

(defun deft-sort-files-by-name (files)
  "Sort FILES alphabetically by non-directory name.
Return the file names in decreasing order."
  (let* ((lst (mapcar (lambda (file)
		       (cons (file-name-nondirectory file) file))
		     files))
	 (lst (sort lst (lambda (x y)
			  (string-lessp (car y) (car x)))))
	 (lst (mapcar 'cdr lst)))
    lst))

(provide 'deft-xapian)

;;; deft-xapian.el ends here
