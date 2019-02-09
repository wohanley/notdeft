;;; notdeft-org8.el --- Org link support for NotDeft notes
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; Some NotDeft-specific support for `org-mode' version 8.  The
;; `org-add-link-type' API is obsolete since Org version 9.

;;; Code:

;; avoid warning if compiling with a later Org version
(declare-function org-add-link-type "org" t t)

(declare-function notdeft-org-complete-deft-link "notdeft-org" t t)

(org-add-link-type
 "deft"
 'notdeft-org-open-deft-link) ;; follow

(defun org-deft-complete-link ()
  "Complete a \"deft:\" link.
Just call `notdeft-org-complete-deft-link'.  Defined for
`org-link-try-special-completion', which expects a specific name
for the link-type-specific completion function."
  (notdeft-org-complete-deft-link))

(org-add-link-type
 "notdeft"
 'notdeft-org-open-notdeft-link) ;; follow

;;; notdeft-org8.el ends here
