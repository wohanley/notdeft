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
When nil, incremental search is limited to
the files in the current `deft-directory'."
  :type 'string
  :safe 'stringp
  :group 'deft)

(defcustom deft-xapian-max-results 100
  "Maximum number of Xapian query results.
\(I.e., '--max-count' for `deft-xapian-program'.)
No limit if nil."
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
  "Xapian query string history.")

(defun deft-xapian-read-query ()
  "Read a Xapian query string, interactively.
Use and update `deft-xapian-query-history' in querying.
Return the read string."
  (let ((s (read-string
	    "Query: " ;; PROMPT
	    nil ;; INITIAL-INPUT
	    deft-xapian-query-history ;; HISTORY
	    nil ;; DEFAULT-VALUE
	    )))
    (when (and s (not (string= s "")))
      s)))

(eval-when-compile (defvar deft-extension))

(defun deft-xapian-index-dirs (dirs &optional async recreate)
  "Create or update a Xapian index for DIRS.
With ASYNC, do the indexing asynchronously.
With RECREATE, truncate any existing index files.
The return value is as for `call-process'."
  (apply
   'call-process
   deft-xapian-program ;; PROGRAM
   nil ;; INFILE
   (if async 0 nil) ;; DESTINATION
   nil ;; DISPLAY
   `("index"
     "--chdir" ,(expand-file-name "." "~")
     ,@(if recreate '("--recreate") nil)
     "--extension" ,(concat "." deft-extension)
     "--lang" ,deft-xapian-language
     ,@(mapcar
	(lambda (dir)
	  (file-relative-name dir "~"))
	dirs))))

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
  (let ((time-sort (if query t deft-xapian-order-by-time))
	(max-results deft-xapian-max-results))
    (when query
      (while (string-match "^ *!\\([[:alpha:]]+\\) +" query)
	(let ((opt (match-string 1 query)))
	  (setq query (substring query (match-end 0)))
	  (pcase (downcase opt)
	    ("time" (setq time-sort t))
	    ("rank" (setq time-sort nil))
	    ("all" (setq max-results nil))
	    ))))
    (let ((s (shell-command-to-string
	      (concat
	       (shell-quote-argument deft-xapian-program) " search"
	       (if time-sort " --time-sort" "")
	       " --lang " (shell-quote-argument deft-xapian-language)
	       (if deft-xapian-boolean-any-case
		   " --boolean-any-case" "")
	       (if deft-xapian-pure-not
		   " --pure-not" "")
	       (if max-results
		   (format " --max-count %d" deft-xapian-max-results)
		 "")
	       (if query
		   (concat " --query " (shell-quote-argument query))
		 "")
	       " " (mapconcat
		    (lambda (dir)
		      (shell-quote-argument
		       (expand-file-name dir "~")))
		    dirs " ")))))
      (mapcar
       (lambda (file)
	 (expand-file-name file "~"))
       (split-string s "\n" t)))))

(provide 'deft-xapian)

;;; deft-xapian.el ends here
