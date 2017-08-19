;;; deft-org9.el --- Org link support for Deft notes

;; Copyright (C) 2017 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "deft.el" for licensing information.

;;; Commentary:
;; Support for managing "deft:" links in `org-mode'.
;; For Org mode version 9 and higher.

;;; Code:

;; The `org-link-set-parameters` API is available since Org version 9.
;; For this functionality to work, ensure you have autoloads set for
;; `deft-open-file-by-notename` and `deft-make-notename-list`.
(org-link-set-parameters
 "deft"
 :follow 'org-deft-open
 :complete 'org-deft-complete-link)

(defun org-deft-open (name)
  "Visit the Deft note with the specified NAME.
This defines the opening of Org \"deft:\" links."
  (deft-open-file-by-notename name))

(defun org-deft-complete-link (&optional pfx)
  "Define completion for an Org \"deft:\" link.
The optional PFX argument is ignored."
  (let ((fn-lst (deft-make-notename-list)))
    ;; `ido` has been a part of Emacs since version 22
    (let ((fn (and fn-lst (ido-completing-read "Deft note: " fn-lst))))
      (concat "deft:" (or fn "")))))

(provide 'deft-org9)

;;; deft-org9.el ends here
