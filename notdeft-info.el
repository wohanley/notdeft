(defun notdeft-pkg-basename ()
  "Read package name and version number.
Read it from the \"notdeft-pkg.el\" file.
Return a string of the form \"NAME-VERSION\"."
  (let ((x (with-temp-buffer
	     (insert-file-contents "notdeft-pkg.el")
	     (read (current-buffer)))))
    (princ (concat (cadr x) "-" (cadr (cdr x))))))
