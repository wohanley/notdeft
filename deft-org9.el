;;; deft-org9.el --- Org link support for Deft notes

;; Copyright (C) 2017 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "deft.el" for licensing information.

;;; Commentary:
;; Some Deft-specific support for `org-mode'.
;; Includes support for managing "deft:" links.
;; For Org mode version 9 and higher.

;; The `org-link-set-parameters' API is available since Org version 9,
;; in the `org' feature. You probably want to load this `deft-org9'
;; feature when you (auto)load Org itself. Deft can be loaded later,
;; as there are autoloads for the required Deft functions.

;;; Code:

(require 'org)
(require 'deft-autoloads)

(org-link-set-parameters
 "deft"
 :follow 'org-deft-open
 :complete 'org-deft-complete-link)

(defun org-deft-open (name)
  "Visit the Deft note with the specified base file NAME.
The argument is a non-directory filename.
This defines the opening of Org \"deft:\" links."
  (deft-open-file-by-basename name))

(defun org-deft-complete-link (&optional pfx)
  "Define completion for Org \"deft:\" links.
The optional PFX argument is ignored."
  (let ((fn-lst (deft-make-basename-list)))
    ;; `ido` has been a part of Emacs since version 22
    (let ((fn (and fn-lst (ido-completing-read "Deft note: " fn-lst))))
      (concat "deft:" (or fn "")))))

(defun deft-insert-org-link (pfx)
  "Insert an Org \"deft:\" link, interactively.
Offer a list of notes from which to choose the link target.
Without a prefix argument, query for a description.
With one prefix argument PFX, include no description.
With two prefix arguments, insert any note title
as the link description. (If multiple notes have the same
name, pick any one of them for title extraction.)"
  (interactive "p")
  (let ((name-lst (deft-make-basename-list)))
    (let ((name (when name-lst
		  (ido-completing-read "Deft note: " name-lst))))
      (when name
	(let* ((file (deft-file-by-basename name))
	       (desc
		(pcase pfx
		  (1 (deft-chomp-nullify
		       (read-string "Description: "
				    (deft-title-from-file-content file)
				    nil nil t)))
		  (4 nil)
		  (16 (deft-title-from-file-content file)))))
	  (if desc
	      (insert "[[deft:" name "][" desc "]]")
	    (insert "[[deft:" name "]]")))))))

(provide 'deft-org9)

;;; deft-org9.el ends here
