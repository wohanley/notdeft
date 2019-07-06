;;; notdeft-org.el --- some support for Org format NotDeft notes
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; Some NotDeft-specific support for `org-mode'.  Includes support for
;; managing "deft:" and "notdeft:" links.  For Org mode version 8 and
;; higher.

;; You probably want to load this `notdeft-org' feature when you load
;; Org itself.

;;; Code:

(require 'org)
(require 'notdeft-autoloads)

(defun notdeft-org-open-deft-link (name)
  "Visit the NotDeft note with the specified base file NAME.
The argument is a non-directory filename.
This defines the opening of Org \"deft:\" links."
  (notdeft-open-file-by-basename name))

(defun notdeft-org-read-deft-link-name ()
  "Query for a \"deft:\" link name.
Do so interactively. Return the name component of a link, without
the \"deft:\" prefix."
  (let ((name-lst (notdeft-make-basename-list)))
    ;; `ido` has been a part of Emacs since version 22
    (when name-lst
      (ido-completing-read "NotDeft note: " name-lst))))

(defun notdeft-org-complete-deft-link (&optional prefix)
  "Define completion for Org \"deft:\" links.
The optional PREFIX argument is ignored."
  (let ((name (notdeft-org-read-deft-link-name)))
    (concat "deft:" (or name ""))))

(defvar notdeft-describe-link 'notdeft-title-from-file-content
  "Function to determine NotDeft note file link description.
The function is given the file name as its sole argument.
Used by `notdeft-select-make-org-link'.")

(defun notdeft-org-read-link-description (&optional desc)
  "Read a link description, interactively.
If DESC is provided, it is used as the initial input. Returns a
string, or nil if no non-whitespace description was provided."
  (notdeft-chomp-nullify
   (read-string "Description: " desc nil nil t)))

(defun notdeft-make-deft-link (name &optional desc)
  "Turn NAME and DESC into a \"deft:\" link.
NAME should be a non-directory file name with extension."
  (org-make-link-string (concat "deft:" name) desc))

(defun notdeft-org-store-deft-link ()
  "Store a \"deft:\" link for the current note.
Like `org-store-link', store the link into `org-stored-links'."
  (interactive)
  (let ((old-file (notdeft-current-filename t t)))
    (when old-file
      (let* ((name (file-name-nondirectory old-file))
	     (link (concat "deft:" name))
	     (desc (notdeft-title-from-file-content old-file)))
	(push (list link desc) org-stored-links)
	(message "Stored: %s" (or desc link))))))

(defun notdeft-org-link-existing-note (notename &optional desc region)
  "Create a \"deft:\" link to an existing note.
Link to a note by NOTENAME, inserting a link description if DESC
is non-nil. Insert the created link at point, unless REGION in
specified \(as a list of two positions), in which case replace
that region. When called interactively: offer a list of notes
from which to choose the link target; query for a note
description, offering to use the text of any active region as the
title, or the result of calling `notdeft-describe-link'
otherwise; use any active region as REGION; if one
\\[universal-argument] is given, then insert a link without DESC;
and if two \\[universal-argument]s are given, use the title of
any note as the description. If multiple notes have the same
NOTENAME, pick any one of them for deriving a description."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (let* ((pfx (prefix-numeric-value current-prefix-arg))
	    (region (when mark-active
		      (list (region-beginning) (region-end))))
	    (desc (and region (= pfx 1)
		       (apply #'buffer-substring-no-properties region)))
	    (name-lst (notdeft-make-basename-list))
	    (name (when name-lst
		    (ido-completing-read "NotDeft note: " name-lst)))
	    (file (and name (notdeft-file-by-basename name)))
	    (desc
	     (unless (= pfx 4)
	       (notdeft-org-read-link-description
		(or desc
		    (when file
		      (pcase pfx
			(1 (notdeft-chomp-nullify
			    (funcall notdeft-describe-link file)))
			(16 (notdeft-title-from-file-content file)))))))))
       (list name desc region))))
  (when notename
    (when region
      (apply #'delete-region region))
    (insert (notdeft-make-deft-link notename desc))))

(defalias 'notdeft-insert-org-link
  #'notdeft-org-link-existing-note
  "Deprecated. Use `notdeft-org-link-existing-note'.")

(defun notdeft-org-link-new-file (&optional dir notename ext data desc region)
  "Create a \"deft:\" link to a new note.
Return the filename of the created file. The arguments DIR,
NOTENAME, EXT, and DATA are as for `notdeft-create-file'. Use
DESC, if any, as the link description. Insert an Org \"deft:\"
link to the newly created note at point, except if REGION is
non-nil, in which case replace that buffer region \(specified as
a list of two position values) with the link. When called
interactively: query for a note title, offering to use the text
of any active region as the title; use any active region as
REGION; derive a NOTENAME based on the title, as usual; use the
default filename extension as EXT; if one \\[universal-argument]
is given, then insert a link without DESC; if two
\\[universal-argument]s are given, the query for a target DIR for
the new note."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (let* ((pfx (prefix-numeric-value current-prefix-arg))
	    (region (when mark-active
		      (list (region-beginning) (region-end))))
	    (title
	     (notdeft-chomp-nullify
	      (read-string "Title: "
			   (when region
			     (notdeft-chomp
			      (apply 'buffer-substring-no-properties region)))
			   nil nil t)))
	    (desc (unless (= pfx 4)
		    (notdeft-org-read-link-description title))))
       (list (and (= pfx 16) 'dir) ;; dir
	     (and title `(title, title)) ;; notename
	     nil ;; ext
	     title ;; data
	     desc
	     region))))
  (let* ((buf (current-buffer))
	 (name (file-name-nondirectory
		(notdeft-create-file dir notename ext data))))
    (switch-to-buffer buf)
    (when region
      (apply 'delete-region region))
    (insert (notdeft-make-deft-link name desc))))

(defalias 'notdeft-link-new-file
  #'notdeft-org-link-new-file
  "Deprecated. Use `notdeft-org-link-new-file'.")

(eval-when-compile
  (defvar notdeft-xapian-query))

(defun notdeft-org-open-notdeft-link (query)
  "Open the NotDeft search specified by QUERY.
This defines the opening of Org \"notdeft:\" links."
  (notdeft-open-query query))

(defun notdeft-org-store-notdeft-link ()
  "Store the current NotDeft search as an Org link.
Use `org-store-link' to invoke this function in a `notdeft-mode'
buffer. Return nil if not in `notdeft-mode', or if there is no
current query."
  (when (and (eq major-mode 'notdeft-mode)
	     notdeft-xapian-query)
    (org-store-link-props
     :type "notdeft"
     :link (concat "notdeft:" notdeft-xapian-query))))

(let ((ver (when (string-match "^[0-9]+" org-version)
	     (string-to-number (match-string 0 org-version)))))
  (load (if (and ver (< ver 9)) "notdeft-org8" "notdeft-org9")))

(provide 'notdeft-org)

;;; notdeft-org.el ends here
