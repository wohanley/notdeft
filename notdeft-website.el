;;; notdeft-website.el --- Website generator for NotDeft
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; Code for turning the readme into a basic web page.

;;; Code:

(require 'ox)

(defun notdeft-web-expand-template (template bindings)
  "Expand TEMPLATE, substituting values for variables.
Variable-value BINDINGS are specified as an association list,
e.g., '((TITLE . \"NotDeft\")). Each template variable name
appearing in the template must be prefixed by the lozenge (◊)
character, as inspired by the Pollen style. No variable name
should be the prefix of another."
  (let ((bindings
	 (mapcar
	  (lambda (x)
	    (cons (concat "◊" (symbol-name (car x))) (cdr x)))
	  bindings)))
    (replace-regexp-in-string
     (regexp-opt (mapcar 'car bindings))
     (lambda (var)
       (let ((binding (assoc var bindings)))
	 (cdr binding)))
     template t t)))

(defun notdeft-web-export-html-file (template bindings file)
  "Export current Org buffer content as HTML.
Produce a full HTML document based on TEMPLATE and BINDINGS for
`notdeft-web-expand-template', but with the symbol `BODYCONTENT'
additionally bound to the HTML converted document body string.
Write the resulting HTML content into FILE."
  (let ((body (org-export-as 'html nil nil t)))
    (let ((doc (notdeft-web-expand-template
		 template (cons `(BODYCONTENT . ,body) bindings))))
      (write-region doc nil file))))

(defun notdeft-web-quote-html (str)
  "Escape chars in STR that are special in HTML."
  (let ((table '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;"))))
    (replace-regexp-in-string
     (regexp-opt (mapcar 'car table))
     (lambda (ch) (cdr (assoc ch table)))
     str t t)))

;; adapted from code by Nicolas Goaziou
;; http://lists.gnu.org/archive/html/emacs-orgmode/2013-05/msg00154.html
(defun notdeft-web-org-property-bindings ()
  "Return an assoc list of Org properties for the buffer."
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword)
      (cons (intern (org-element-property :key keyword))
	    (notdeft-web-quote-html (org-element-property :value keyword))))))

(defun notdeft-web-make (in-file template out-file)
  "Build a homepage.
Transform Org IN-FILE, in terms of a HTML TEMPLATE, into HTML
OUT-FILE."
  (with-temp-buffer
    (insert-file-contents in-file)
    (notdeft-web-export-html-file
      (with-temp-buffer
	(insert-file-contents template)
	(buffer-string))
      (notdeft-web-org-property-bindings)
      out-file)
    out-file))

(provide 'notdeft-website)

;;; notdeft-website.el ends here
