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

(defun notdeft-org-complete-deft-link (&optional pfx)
  "Define completion for Org \"deft:\" links.
The optional PFX argument is ignored."
  (let ((fn-lst (notdeft-make-basename-list)))
    ;; `ido` has been a part of Emacs since version 22
    (let ((fn (and fn-lst (ido-completing-read "NotDeft note: " fn-lst))))
      (concat "deft:" (or fn "")))))

(defvar notdeft-describe-link 'notdeft-title-from-file-content
  "Function to determine NotDeft note file link description.
The function is given the file name as its sole argument.
Used by `notdeft-select-make-org-link'.")

(defun notdeft-make-deft-link (name &optional desc)
  "Turn NAME and DESC into a \"deft:\" link.
NAME should be a non-directory file name with extension."
  (org-make-link-string (concat "deft:" name) desc))

(defun notdeft-select-make-org-link (pfx)
  "Return an Org \"deft:\" link as a string.
Choose the link target interactively.
The PFX argument is as for `notdeft-insert-org-link'."
  (let ((name-lst (notdeft-make-basename-list)))
    (let ((name (when name-lst
		  (ido-completing-read "NotDeft note: " name-lst))))
      (when name
	(let* ((file (notdeft-file-by-basename name))
	       (desc
		(pcase pfx
		  (1 (notdeft-chomp-nullify
		       (read-string "Description: "
				    (funcall notdeft-describe-link file)
				    nil nil t)))
		  (4 nil)
		  (16 (notdeft-title-from-file-content file)))))
	  (notdeft-make-deft-link name desc))))))

(defun notdeft-insert-org-link (pfx)
  "Insert an Org \"deft:\" link, interactively.
Offer a list of notes from which to choose the link target.
Without a prefix argument, query for a description.
With one prefix argument PFX, include no description.
With two prefix arguments, insert any note title
as the link description. (If multiple notes have the same
name, pick any one of them for title extraction.)"
  (interactive "*p")
  (let ((s (notdeft-select-make-org-link pfx)))
    (when s
      (insert s))))

(defun notdeft-link-new-file (pfx)
  "Create a \"deft:\" link to a new note.
Query for a note title and link description. Offer to use the
text of any active region as the title. Derive a note filename
based on the title, as usual. Insert an Org \"deft:\" link to the
newly created note at point. Return the filename of the created
file. The prefix argument PFX is as for `notdeft-new-file'."
  (interactive "*P")
  (let* ((buf (current-buffer))
	 (region (when mark-active
		   (list (region-beginning) (region-end))))
	 (title
	  (read-string "Title: "
		       (if region
			   (notdeft-chomp
			    (apply 'buffer-substring-no-properties region))
			 "") nil nil t))
	 (desc
	  (notdeft-chomp-nullify
	   (read-string "Description: " title nil nil t)))
	 (name (file-name-nondirectory
		(notdeft-new-file-named pfx title))))
    (switch-to-buffer buf)
    (when region
      (apply 'delete-region region))
    (insert (notdeft-make-deft-link name desc))))

(defun notdeft-kill-ring-save-org-link (pfx)
  "Store an Org \"deft:\" link into `kill-ring'.
The PFX argument is as for `notdeft-insert-org-link'."
  (interactive "p")
  (let ((s (notdeft-select-make-org-link pfx)))
    (when s
      (kill-new s))))

(eval-when-compile
  (defvar notdeft-xapian-query))

(defun notdeft-org-open-notdeft-link (query)
  "Open the NotDeft search specified by QUERY.
This defines the opening of Org \"notdeft:\" links."
  (notdeft-open-query query))

(defun notdeft-org-store-notdeft-link ()
  "Store the current NotDeft search as an Org link.
This only works in a `notdeft-buffer'.
Return nil otherwise."
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
