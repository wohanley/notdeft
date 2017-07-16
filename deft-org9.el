;; For Org mode version 9 and higher.

;; The `org-link-set-parameters` API is available since Org version 9.
;; For this functionality to work, ensure you have autoloads set for
;; `deft-open-file-by-notename` and `deft-make-notename-list`.
(org-link-set-parameters
 "deft"
 :follow 'org-deft-open
 :complete 'org-deft-complete-link)

(defun org-deft-open (name)
  "Visit the Deft note with the specified NAME.
Defines the behavior of C-c C-o on `deft:` links."
  (deft-open-file-by-notename name))

(defun org-deft-complete-link (&optional pfx)
  "Defines the behavior of `deft:` link completion in Org.
(Initiate completion with Tab after C-c C-l.)
The optional PFX argument is ignored."
  (let ((fn-lst (deft-make-notename-list)))
    ;; `ido` has been a part of Emacs since version 22
    (let ((fn (and fn-lst (ido-completing-read "Deft note: " fn-lst))))
      (concat "deft:" (or fn "")))))

(provide 'deft-org9)
