;;; notdeft-org9.el --- Org link support for NotDeft notes

;; Copyright (C) 2017 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; Some NotDeft-specific support for `org-mode'.
;; Includes support for managing "deft:" links.
;; For Org mode version 9 and higher.

;; The `org-link-set-parameters' API is available since Org version 9,
;; in the `org' feature. You probably want to load this `notdeft-org9'
;; feature when you (auto)load Org itself. NotDeft can be loaded later,
;; and will be if you have loaded `notdeft-autoloads'.

;;; Code:

(require 'org)

(eval-when-compile
  ;; avoid warning if compiling with an earlier Org version
  (autoload 'org-link-set-parameters "org")
  
  (autoload 'notdeft-open-file-by-basename "notdeft")
  (autoload 'notdeft-make-basename-list "notdeft")
  (autoload 'notdeft-file-by-basename "notdeft")
  (autoload 'notdeft-chomp-nullify "notdeft")
  (autoload 'notdeft-title-from-file-content "notdeft"))

(org-link-set-parameters
 "deft"
 :follow #'notdeft-org-open-deft-link
 :complete #'notdeft-org-complete-deft-link)

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

(defun notdeft-make-org-link (pfx)
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
				    (notdeft-title-from-file-content file)
				    nil nil t)))
		  (4 nil)
		  (16 (notdeft-title-from-file-content file)))))
	  (if desc
	      (concat "[[deft:" name "][" desc "]]")
	    (concat "[[deft:" name "]]")))))))

(defun notdeft-insert-org-link (pfx)
  "Insert an Org \"deft:\" link, interactively.
Offer a list of notes from which to choose the link target.
Without a prefix argument, query for a description.
With one prefix argument PFX, include no description.
With two prefix arguments, insert any note title
as the link description. (If multiple notes have the same
name, pick any one of them for title extraction.)"
  (interactive "p")
  (let ((s (notdeft-make-org-link pfx)))
    (when s
      (insert s))))

(defun notdeft-kill-ring-save-org-link (pfx)
  "Store an Org \"deft:\" link into `kill-ring'.
The PFX argument is as for `notdeft-insert-org-link'."
  (interactive "p")
  (let ((s (notdeft-make-org-link pfx)))
    (when s
      (kill-new s))))

(eval-when-compile
  (defvar notdeft-xapian-query)
  (autoload 'notdeft-open-query "notdeft"))

(org-link-set-parameters
 "notdeft"
 :follow #'notdeft-org-open-notdeft-link
 :store #'notdeft-org-store-notdeft-link)

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

(provide 'notdeft-org9)

;;; notdeft-org9.el ends here
