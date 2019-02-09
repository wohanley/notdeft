;;; notdeft-org9.el --- Org link support for NotDeft notes
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; Some NotDeft-specific support for `org-mode' version 9.  The
;; `org-link-set-parameters' API is available since Org version 9, in
;; the `org' feature.

;;; Code:

;; avoid warning if compiling with an earlier Org version
(declare-function org-link-set-parameters "org" t t)
  
(org-link-set-parameters
 "deft"
 :follow 'notdeft-org-open-deft-link
 :complete 'notdeft-org-complete-deft-link)

(org-link-set-parameters
 "notdeft"
 :follow 'notdeft-org-open-notdeft-link
 :store 'notdeft-org-store-notdeft-link)

;;; notdeft-org9.el ends here
