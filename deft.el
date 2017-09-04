;;; deft.el --- quickly browse, filter, and edit plain text notes
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2011 Jason R. Blevins <jrblevin@sdf.org>
;; Copyright (C) 2011-2017 Tero Hasu <tero@hasu.is>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation  and/or other materials provided with the distribution.
;; 3. Neither the names of the copyright holders nor the names of any
;;    contributors may be used to endorse or promote products derived from
;;    this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;; Author: Jason R. Blevins <jrblevin@sdf.org>
;; Author: Tero Hasu <tero@hasu.is>
;; Keywords: plain text, notes, Simplenote, Notational Velocity

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Deft is an Emacs mode for quickly browsing, filtering, and editing
;; directories of plain text notes, inspired by Notational Velocity.
;; It was designed for increased productivity when writing and taking
;; notes by making it fast and simple to find the right file at the
;; right time and by automating many of the usual tasks such as
;; creating new files and saving files.

;; Deft is open source software and may be freely distributed and
;; modified under the BSD license.  This version is a fork of
;; Deft version 0.3, which was released on September 11, 2011.

;; ![File Browser](http://jblevins.org/projects/deft/browser.png)

;; The Deft buffer is simply a file browser which lists the titles of
;; all text files in the Deft directory (or directories) followed by
;; short summaries and last modified times. The title is taken to be
;; the first line of the file (or as specified by an Org "TITLE" file
;; property) and the summary is extracted from the text that follows.
;; Files are sorted in terms of the last modified date, from newest to
;; oldest.

;; All Deft files or notes are simple plain text files (or Org markup
;; files). As an example, the following directory structure generated
;; the screenshot above.
;;
;;     % ls ~/.deft
;;     about.org    browser.org     directory.org   operations.org
;;     ack.org      completion.org  extensions.org  text-mode.org
;;     binding.org  creation.org    filtering.org
;;
;;     % cat ~/.deft/about.org
;;     About
;;
;;     An Emacs mode for slicing and dicing plain text files.

;; ![Filtering](http://jblevins.org/projects/deft/filter.png)

;; Deft's primary operation is searching and filtering.  The list of
;; files can be limited or filtered using a search string, which will
;; match both the title and the body text.  To initiate a filter,
;; simply start typing.  Filtering happens on the fly.  As you type,
;; the file browser is updated to include only files that match the
;; current string.

;; To open the first matching file, simply press `RET`.  If no files
;; match your search string, pressing `RET` will create a new file
;; using the string as the title.  This is a very fast way to start
;; writing new notes.  The filename will be generated automatically.

;; To open files other than the first match, navigate up and down
;; using `C-p` and `C-n` and press `RET` on the file you want to open.

;; Press `C-c C-c` to clear the filter string and display all files
;; and `C-c C-g` to refresh the file browser using the current filter
;; string.

;; Static filtering is also possible by pressing `C-c C-l`.  This is
;; sometimes useful on its own, and it may be preferable in some
;; situations, such as over slow connections or on older systems,
;; where interactive filtering performance is poor.

;; Common file operations can also be carried out from within Deft.
;; Files can be renamed using `C-c C-r` or deleted using `C-c C-d`.
;; New files can also be created using `C-c C-n` for quick creation or
;; `C-c C-m` for a filename prompt.  You can leave Deft at any time
;; with `C-c C-q`.

;; Archiving unused files can be carried out by pressing `C-c C-a`.
;; Files will be moved to `deft-archive-directory' within your
;; chosen `deft-directory'.

;; Files opened with Deft can be automatically saved after Emacs has
;; been idle for a customizable number of seconds. To enable this
;; feature, configure `deft-auto-save-interval' with the desired
;; floating point value.

;; Getting Started
;; ---------------

;; To start using it, place it somewhere in your Emacs load-path and
;; add the line

;;     (require 'deft)

;; in your `.emacs` file.  Then run `M-x deft` to start.  It is useful
;; to create a global keybinding for the `deft` function (e.g., a
;; function key) to start it quickly (see below for details).

;; One useful way to use Deft is to keep a directory of notes in a
;; Dropbox folder.  This can be used with other applications and
;; mobile devices, for example, Notational Velocity or Simplenote
;; on OS X, Elements on iOS, or Epistle on Android.

;; Customization
;; -------------

;; Customize the `deft` group to change the functionality.

;; By default, Deft looks for notes by searching for files with the
;; extension `.org` in the `~/.deft` directory.  You can customize
;; both the file extension and the Deft note search path by running
;; `M-x customize-group` and typing `deft`.  Alternatively, you can
;; configure them in your `.emacs` file:

;;     (setq deft-extension "txt")
;;     (setq deft-secondary-extensions '("md" "scrbl"))
;;     (setq deft-path '("~/.deft/" "~/Dropbox/notes/"))

;; The variable `deft-extension' specifies the default extension
;; for new notes. There can be `deft-secondary-extensions' for
;; files that are also considered to be Deft notes.

;; While you can choose a `deft-extension' that is not ".org",
;; this fork of Deft is somewhat optimized to working with
;; files in Org format.

;; You can easily set up a global keyboard binding for Deft.  For
;; example, to bind it to F8, add the following code to your `.emacs`
;; file:

;;     (global-set-key [f8] 'deft)

;; The faces used for highlighting various parts of the screen can
;; also be customized.  By default, these faces inherit their
;; properties from the standard font-lock faces defined by your current
;; color theme.

;; Acknowledgments
;; ---------------

;; Thanks to Konstantinos Efstathiou for writing simplnote.el, from
;; which I borrowed liberally, and to Zachary Schneirov for writing
;; Notational Velocity, which I have never had the pleasure of using,
;; but whose functionality and spirit I wanted to bring to other
;; platforms, such as Linux, via Emacs.

;; History
;; -------

;; Version 0.3x:

;; * Most notably, add a Xapian-based query engine.
;; * Add support for multiple notes directories.

;; Version 0.3 (2011-09-11):

;; * Internationalization: support filtering with multibyte characters.

;; Version 0.2 (2011-08-22):

;; * Match filenames when filtering.
;; * Automatically save opened files (optional).
;; * Address some byte-compilation warnings.

;; Deft was originally written by Jason Blevins.
;; The initial version, 0.1, was released on August 6, 2011.

;;; Code:

(require 'cl-lib)
(require 'widget)
(require 'wid-edit)
(require 'deft-xapian)

;; Customization

(defgroup deft nil
  "Emacs Deft mode."
  :group 'local)

(defcustom deft-path '("~/.deft/")
  "Deft directory search path.
A list of directories which may or may not exist on startup.
Normally a list of strings, but may also contain other sexps,
which are evaluated at startup or by calling `deft-refresh'.
Each sexp must evaluate to a string or a list of strings
naming directories."
  :type '(repeat (string :tag "Directory"))
  :safe (lambda (lst) (cl-every 'deft-safe-path-element-p lst))
  :group 'deft)

(defcustom deft-extension "org"
  "Default Deft file extension."
  :type 'string
  :safe 'stringp
  :group 'deft)

(defcustom deft-secondary-extensions nil
  "Additional Deft file extensions."
  :type '(repeat string)
  :safe (lambda (lst) (cl-every 'stringp lst))
  :group 'deft)

(defcustom deft-archive-directory "_archive"
  "Sub-directory name for archived notes."
  :type 'string
  :safe 'stringp
  :group 'deft)

(defcustom deft-auto-save-interval 0.0
  "Idle time before automatically saving buffers opened by Deft.
Specified as a number of seconds.
Set to zero to disable."
  :type 'float
  :safe 'floatp
  :group 'deft)

(defcustom deft-time-format " %Y-%m-%d %H:%M"
  "Format string for modification times in the Deft browser.
Set to nil to hide."
  :type '(choice (string :tag "Time format")
		 (const :tag "Hide" nil))
  :safe 'string-or-null-p
  :group 'deft)

;; Faces

(defgroup deft-faces nil
  "Faces used in Deft mode"
  :group 'deft
  :group 'faces)

(defface deft-header-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for Deft header."
  :group 'deft-faces)

(defface deft-filter-string-face
  '((t :inherit font-lock-string-face))
  "Face for Deft filter string."
  :group 'deft-faces)

(defface deft-title-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for Deft file titles."
  :group 'deft-faces)

(defface deft-separator-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Face for Deft separator string."
  :group 'deft-faces)

(defface deft-summary-face
  '((t :inherit font-lock-comment-face))
  "Face for Deft file summary strings."
  :group 'deft-faces)

(defface deft-time-face
  '((t :inherit font-lock-variable-name-face))
  "Face for Deft last modified times."
  :group 'deft-faces)

;; Constants

(defconst deft-version "0.3x")

(defconst deft-buffer "*Deft*"
  "Deft buffer name.")

(defconst deft-separator " --- "
  "Text used to separate file titles and summaries.")

;; Global variables

(defvar deft-directory nil
  "Chosen Deft data directory.")

(defvar deft-mode-hook nil
  "Hook run when entering Deft mode.")

(defvar deft-xapian-query nil
  "Current Xapian query string.")

(defvar deft-filter-regexp nil
  "Current filter regexp used by Deft.")

(defvar deft-current-files nil
  "List of files matching current filter.")

(defvar deft-all-files nil
  "List of all files to list or filter.")

(defvar deft-directories nil
  "A cache of Deft directories corresponding to `deft-path'.
May not have been initialized if nil.")

(defvar deft-hash-contents nil
  "Hash containing complete cached file contents, keyed by filename.")

(defvar deft-hash-mtimes nil
  "Hash containing cached file modification times, keyed by filename.")

(defvar deft-hash-titles nil
  "Hash containing cached file titles, keyed by filename.")

(defvar deft-hash-summaries nil
  "Hash containing cached file summaries, keyed by filename.")

(defvar deft-auto-save-buffers nil
  "List of buffers that will be automatically saved.")

(defvar deft-window-width nil
  "Width of Deft buffer.")

;; Deft path and directory

(defun deft-safe-path-element-p (x)
  "Whether X is a safe `deft-path' element."
  (or (stringp x)
      (symbolp x)
      (and x (listp x)
	   (memq (car x)
		 '(append
		   car cdr concat cons
		   expand-file-name
		   file-name-as-directory
		   file-name-directory
		   file-name-extension
		   file-name-nondirectory
		   file-name-sans-extension
		   file-expand-wildcards
		   format list reverse))
	   (cl-every 'deft-safe-path-element-p (cdr x)))))

(defun deft-resolve-directories ()
  "Resolve directories from `deft-path'.
Return the result as a list of strings."
  (apply 'append
	 (mapcar
	  (lambda (x)
	    (cond
	     ((stringp x)
	      (list x))
	     ((or (symbolp x) (listp x))
	      (let ((y (eval x)))
		(cond
		 ((stringp y) (list y))
		 ((and (listp y) (cl-every 'stringp y)) y)
		 (t (error "Expected string or list thereof: %S" y)))))
	     (t (error "Path element: %S" x))))
	  deft-path)))

(defun deft-get-directories ()
  "Return `deft-directories', computing it if nil."
  (unless deft-directories
    (setq deft-directories (deft-resolve-directories)))
  deft-directories)

;; File processing

(defun deft-title-to-notename (str)
  "Turn a title string STR to a note name string."
  (when (string-match "^[^a-zA-Z0-9-]+" str)
    (setq str (replace-match "" t t str)))
  (when (string-match "[^a-zA-Z0-9-]+$" str)
    (setq str (replace-match "" t t str)))
  (while (string-match "[^a-zA-Z0-9-]+" str)
    (setq str (replace-match "-" t t str)))
  (setq str (downcase str))
  str)

(defun deft-format-time-for-filename (tm)
  "Format time TM suitably for filenames."
  (format-time-string "%Y-%m-%d-%H-%M-%S" tm t)) ; UTC

(defun deft-generate-filename (&optional ext dir)
  "Generate a new unique filename.
Do so without being given any information about
note title or content.
Have the file have the extension EXT, and
be in directory DIR."
  (let (filename)
    (while (or (not filename)
	       (file-exists-p filename))
      (let* ((ctime (current-time))
	     (ctime-s (deft-format-time-for-filename ctime))
	     (base-filename (format "Deft--%s" ctime-s)))
	(setq filename (deft-make-filename base-filename ext dir))))
    filename))

(defun deft-make-filename (notename &optional ext dir in-subdir)
  "Derive a filename from Deft note name NOTENAME.
The filename shall have the extension EXT,
defaulting to `deft-extension'.
The file shall reside in the directory DIR,
defaulting to `deft-directory', except that
IN-SUBDIR indicates that the file should be given
its own subdirectory."
  (let ((root (or dir deft-directory)))
    (concat (file-name-as-directory root)
	    (if in-subdir (file-name-as-directory notename) "")
	    notename "." (or ext deft-extension))))

(defun deft-make-file-re ()
  "Return a regexp matching strings with a Deft extension."
  (let ((exts (cons deft-extension deft-secondary-extensions)))
    (concat "\\.\\(?:"
	    (mapconcat 'regexp-quote exts "\\|")
	    "\\)$")))

(defun deft-strip-extension (file)
  "Strip any Deft filename extension from FILE."
  (replace-regexp-in-string (deft-make-file-re) "" file))
  
(defun deft-base-filename (file)
  "Strip the leading path and Deft extension from filename FILE.
Use `file-name-directory' to get the directory component.
Strip any extension with `deft-strip-extension'."
  (let* ((file (file-name-nondirectory file))
	 (file (deft-strip-extension file)))
    file))

(defun deft-basename-from-file (file)
  "Extract the basename of the note FILE."
  (file-name-nondirectory file))

(defun deft-file-readable-p (file)
  "Whether FILE is a readable non-directory."
  (and (file-readable-p file)
       (not (file-directory-p file))))

(defun deft-read-file (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;;###autoload
(defun deft-title-from-file-content (file)
  "Extract a title from FILE content.
Return nil on failure."
  (when (deft-file-readable-p file)
    (let* ((contents (deft-read-file file))
	   (title (deft-parse-title file contents)))
      title)))

(defun deft-chomp (str)
  "Trim leading and trailing whitespace from STR."
  (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)"
			    "" str))

;;;###autoload
(defun deft-file-by-basename (name)
  "Resolve a Deft note NAME to a full pathname.
NAME is a non-directory filename, with extension.
Resolve it to the path of a file under a `deft-path'
directory, if such a note file does exist.
If multiple such files exist, return one of them.
If none exist, return nil."
  (deft-ensure-init)
  (let* ((file-p (lambda (pn)
		   (string= name (file-name-nondirectory pn))))
	 (cand-roots (deft-get-directories))
	 result)
    (while (and cand-roots (not result))
      (let ((abs-root (expand-file-name (car cand-roots))))
	(setq cand-roots (cdr cand-roots))
	(setq result (deft-root-find-file file-p abs-root))))
    result))

(defun deft-root-find-file (file-p abs-dir)
  "Find a file matching predicate FILE-P under ABS-DIR.
ABS-DIR is assumed to be a Deft root.
Return nil if no matching file is found."
  (and
   (file-readable-p abs-dir)
   (file-directory-p abs-dir)
   (let ((abs-dir (file-name-as-directory abs-dir))
	 (files (directory-files abs-dir nil "^[^._#]" t))
	 result)
     (while (and files (not result))
       (let* ((abs-file (concat abs-dir (car files))))
	 (setq files (cdr files))
	 (cond
	  ((file-directory-p abs-file)
	   (setq result (deft-root-find-file file-p abs-file)))
	  ((funcall file-p abs-file)
	   (setq result abs-file)))))
     result)))

(defun deft-glob (root &optional dir result file-re)
  "Return a list of all Deft files in a directory tree.
List the Deft files under the specified Deft ROOT and
its directory DIR, with DIR given as a path relative
to the directory ROOT.
If DIR is nil, then list Deft files under ROOT.
Add to the RESULT list in an undefined order,
and return the resulting value.
Only include files matching regexp FILE-RE."
  (let* ((root (file-name-as-directory (expand-file-name root)))
	 (dir (file-name-as-directory (or dir ".")))
	 (abs-dir (expand-file-name dir root)))
    (and
     (file-readable-p abs-dir)
     (file-directory-p abs-dir)
     (let* ((files (directory-files abs-dir nil "^[^._#]" t))
	    (file-re (or file-re (deft-make-file-re))))
       (dolist (file files result)
	 (let* ((rel-file (file-relative-name
			   (expand-file-name file abs-dir)
			   root))
		(abs-file (concat root rel-file)))
	   (cond
	    ((file-directory-p abs-file)
	     (setq result (deft-glob root rel-file result file-re)))
	    ((string-match-p file-re file)
	     (setq result (cons rel-file result))))))))))

(defun deft-glob/absolute (root &optional dir result file-re)
  "Like `deft-glob', but return the results as absolute paths.
The arguments ROOT, DIR, RESULT, and FILE-RE are the same."
  (mapcar
   (lambda (rel)
     (expand-file-name rel root))
   (deft-glob root dir result file-re)))

(defun deft-find-all-files-in-dir (directory full)
  "Return a list of all Deft files under DIRECTORY.
The specified DIRECTORY must be a Deft root.
Return an empty list if there is no readable DIRECTORY.
Return the files' absolute paths if FULL is true."
  (if full
      (deft-glob/absolute directory)
    (deft-glob directory)))

;;;###autoload
(defun deft-make-basename-list ()
  "Return the names of all Deft notes.
Search all existing `deft-path' directories.
The result list is sorted by the `string-lessp' relation.
It may contain duplicates."
  (deft-ensure-init)
  (let ((dir-lst (deft-get-directories))
	(fn-lst '()))
    (dolist (dir dir-lst)
      (setq fn-lst
	    (append fn-lst
		    (deft-find-all-files-in-dir dir t))))
    ;; `sort` may modify `name-lst`
    (let ((name-lst (mapcar 'deft-basename-from-file fn-lst)))
      (sort name-lst 'string-lessp))))

(defun deft-parse-title (file contents)
  "Parse the given FILE CONTENTS and determine the title.
The title is taken to be the first non-empty line of a file.
Org comments are skipped, and \"#+TITLE\" syntax is recognized,
and may also be used to define the title.
Returns nil if there is no non-empty, not-just-whitespace
title in CONTENTS."
  (let* ((res (with-temp-buffer
		(insert contents)
		(deft-parse-buffer)))
	 (title (car res)))
    title))

(defun deft-substring-from (str from max-n)
  "Extract a substring from STR.
Extract it from position FROM, and up to MAX-N characters."
  (substring str from (max (length str) (+ from max-n))))

(defun deft-condense-whitespace (str)
  "Condense whitespace in STR into a single space."
  (replace-regexp-in-string "[[:space:]\n]+" " " str))

;;;###autoload
(defun deft-chomp-nullify (str &optional trim)
  "Return string STR if non-empty, otherwise return nil.
Optionally, use function TRIM to trim any result string."
  (when str
    (let ((str (deft-chomp str)))
      (unless (string= "" str)
	(if trim (funcall trim str) str)))))

(defun deft-parse-buffer ()
  "Parse the file contents in the current buffer.
Extract a title and summary.
The summary is a string extracted from the contents following the
title. The result is a list (TITLE SUMMARY KEYWORDS) where any
component may be nil. The result list may include additional,
undefined components."
  (let (title summary keywords dbg (end (point-max)))
    (save-match-data
      (save-excursion
	(goto-char (point-min))
	(while (and (< (point) end) (not (and title summary)))
	  ;;(message "%S" (list (point) title summary))
	  (cond
	   ((looking-at "^#\\+TITLE:[ \t]*\\(.*\\)$") ;; Org title
	    (setq dbg (cons `(TITLE . ,(match-string 1)) dbg))
	    (setq title (match-string 1))
	    (goto-char (match-end 0)))
	   ((looking-at "^#\\+\\(?:KEYWORDS\\|FILETAGS\\):[ \t]*\\(.*\\)$")
	    (setq dbg (cons `(KEYWORDS . ,(match-string 1)) dbg))
	    (setq keywords (match-string 1))
	    (goto-char (match-end 0)))
	   ((looking-at "^#.*$") ;; line comment
	    (setq dbg (cons `(COMMENT . ,(match-string 0)) dbg))
	    (goto-char (match-end 0)))
	   ((looking-at "[[:graph:]].*$") ;; non-whitespace
	    (setq dbg (cons `(REST . ,(match-string 0)) dbg))
	    (unless title
	      (setq title (match-string 0))
	      (goto-char (match-end 0)))
	    (setq summary (buffer-substring (point) end))
	    (goto-char end))
	   (t
	    (let* ((b (point)) (e (+ b 1)))
	      (setq dbg (cons `(SKIP . ,(buffer-substring b e)) dbg))
	      (goto-char e)))))))
    (list
     (deft-chomp-nullify title)
     (deft-chomp-nullify summary 'deft-condense-whitespace)
     (deft-chomp-nullify keywords)
     dbg)))

(defun deft-cache-remove-file (file)
  "Remove FILE from the cache.
Do nothing if FILE is not in the cache."
  (remhash file deft-hash-mtimes)
  (remhash file deft-hash-titles)
  (remhash file deft-hash-summaries)
  (remhash file deft-hash-contents))

(defun deft-cache-clear ()
  "Clear the cache of file information."
  (clrhash deft-hash-mtimes)
  (clrhash deft-hash-titles)
  (clrhash deft-hash-summaries)
  (clrhash deft-hash-contents))

(defun deft-cache-gc ()
  "Remove obsolete entries from the cache.
That is, remove information for files that no longer exist.
Return a list of the files whose information was removed."
  (let (lst)
    (maphash (lambda (file v)
	       (unless (file-exists-p file)
		 (setq lst (cons file lst))))
	     deft-hash-mtimes)
    (dolist (file lst lst)
      (deft-cache-remove-file file))))

(defun deft-cache-file (file)
  "Update file cache for FILE.
Keep any information for a non-existing file."
  (when (file-exists-p file)
    (let ((mtime-cache (deft-file-mtime file))
          (mtime-file (nth 5 (file-attributes file))))
      (when (or (not mtime-cache)
		(time-less-p mtime-cache mtime-file))
	(deft-cache-newer-file file mtime-file)))))

(defun deft-cache-newer-file (file mtime)
  "Update cached information for FILE with given MTIME."
  (let* ((res (with-temp-buffer
		(insert-file-contents file)
		(deft-parse-buffer)))
	 (title (car res))
	 (summary (cadr res))
	 (contents
	  (concat file " "
		  (or title "") " "
		  (or (car (cddr res)) "") " "
		  (or summary ""))))
    (puthash file mtime deft-hash-mtimes)
    (puthash file title deft-hash-titles)
    (puthash file summary deft-hash-summaries)
    (puthash file contents deft-hash-contents)))

(defun deft-file-newer-p (file1 file2)
  "Return non-nil if FILE1 was modified since FILE2 and nil otherwise."
  (let (time1 time2)
    (setq time1 (deft-file-mtime file1))
    (setq time2 (deft-file-mtime file2))
    (time-less-p time2 time1)))

(defun deft-cache-initialize ()
  "Initialize hash tables for caching files."
  (setq deft-hash-contents (make-hash-table :test 'equal))
  (setq deft-hash-mtimes (make-hash-table :test 'equal))
  (setq deft-hash-titles (make-hash-table :test 'equal))
  (setq deft-hash-summaries (make-hash-table :test 'equal)))

;; Cache access

(defun deft-file-contents (file)
  "Retrieve complete contents of FILE from cache."
  (gethash file deft-hash-contents))

(defun deft-file-mtime (file)
  "Retrieve modified time of FILE from cache."
  (gethash file deft-hash-mtimes))

(defun deft-file-title (file)
  "Retrieve title of FILE from cache."
  (gethash file deft-hash-titles))

(defun deft-file-summary (file)
  "Retrieve summary of FILE from cache."
  (gethash file deft-hash-summaries))

;; File list display

(defun deft-print-header ()
  "Prints the *Deft* buffer header."
  (widget-insert
   (propertize "Deft: " 'face 'deft-header-face))
  (when deft-xapian-query
    (widget-insert
     (propertize (concat deft-xapian-query ": ")
		 'face 'deft-xapian-query-face)))
  (when deft-filter-regexp
    (widget-insert
     (propertize deft-filter-regexp 'face 'deft-filter-string-face)))
  (widget-insert "\n\n"))

(eval-when-compile
  (defvar deft-mode-map))

(defun deft-buffer-setup ()
  "Render the file browser in the `deft-buffer'."
  (setq deft-window-width (window-width))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (deft-print-header)

  ;; Print the files list
  (if (not (file-exists-p deft-directory))
      (widget-insert (deft-no-directory-message))
    (if deft-current-files
	(mapc 'deft-file-widget deft-current-files) ;; for side effects
      (widget-insert (deft-no-files-message))))

  (use-local-map deft-mode-map)
  (widget-setup)
  (goto-char 1)
  (forward-line 2))

(defun deft-file-widget (file)
  "Add a line to the file browser for the given FILE."
  (let* ((text (deft-file-contents file))
	 (title (deft-file-title file))
	 (summary (deft-file-summary file))
	 (mtime (and deft-time-format
		     (format-time-string deft-time-format
					 (deft-file-mtime file))))
	 (mtime-width (length mtime))
	 (line-width (- deft-window-width mtime-width))
	 (title-width (min line-width (length title)))
	 (summary-width (min (length summary)
			     (- line-width
				title-width
				(length deft-separator)))))
    (widget-create 'link
		   :button-prefix ""
		   :button-suffix ""
		   :button-face 'deft-title-face
		   :format "%[%v%]"
		   :tag file
		   :help-echo "Edit this file"
		   :notify (lambda (widget &rest ignore)
			     (deft-open-file (widget-get widget :tag)))
		   (if title
		       (substring title 0 title-width)
		     "[Empty file]"))
    (when (> summary-width 0)
      (widget-insert (propertize deft-separator 'face 'deft-separator-face))
      (widget-insert (propertize
		      (if summary (substring summary 0 summary-width) "")
		      'face 'deft-summary-face)))
    (when mtime
      (while (< (current-column) line-width)
	(widget-insert " "))
      (widget-insert (propertize mtime 'face 'deft-time-face)))
    (widget-insert "\n")))

(add-hook 'window-configuration-change-hook
	  (lambda ()
	    (when (and (eq (current-buffer) (get-buffer deft-buffer))
                       (not (eq deft-window-width (window-width))))
              (deft-buffer-setup))))

(defun deft-keep-readable (files)
  "Filter out unreadable FILES."
  (let (result)
    (dolist (file files result)
      (when (file-readable-p file)
	(setq result (cons file result))))))

(defun deft-files-under-root (dir)
  "Return a list of all readable Deft files under DIR.
Return the results as absolute paths.
The DIR argument must be a Deft root directory."
  (deft-keep-readable (deft-find-all-files-in-dir dir t)))

(defun deft-cache-update (files)
  "Update cached information for FILES."
  (mapc 'deft-cache-file files))

(defun deft-xapian-index-files (files)
  "Update Xapian index for FILES (at least)."
  (let ((dirs
	 (delete-dups
	  (mapcar 'deft-dir-of-deft-file files))))
    (deft-xapian-index-dirs dirs)))

(defun deft-xapian-re-index ()
  "Recreate all Xapian indexes on `deft-path'."
  (interactive)
  (when deft-xapian-program
    (deft-xapian-index-dirs (deft-get-directories) nil t)
    (deft-changed 'nothing)))

(defun deft-changed (what &optional things)
  "Refresh Deft file list, cache, and search index state.
The arguments hint at what may need refreshing.

WHAT is a symbolic hint for purposes of optimization.
It is one of:
- `nothing' to assume no filesystem changes;
- `dirs' to assume changes in THINGS Deft directories;
- `files' to assume changes in THINGS Deft files; or
- `anything' to make no assumptions about filesystem changes.

As appropriate, refresh both file information cache and
any Xapian indexes, and update `deft-all-files' and
`deft-current-file' lists to reflect those changes,
or changes to `deft-filter-regexp' or `deft-xapian-query'."
  (if (not deft-xapian-program)
      (unless (eq what 'nothing)
	(setq deft-all-files (deft-files-under-root deft-directory))
	(deft-cache-update deft-all-files)
	(setq deft-all-files (deft-sort-files deft-all-files)))
    (let ((roots (deft-get-directories)))
      (cl-case what
	(anything (deft-xapian-index-dirs roots))
	(dirs (deft-xapian-index-dirs things))
	(files (deft-xapian-index-files things)))
      (setq deft-all-files (deft-xapian-search roots deft-xapian-query))
      (deft-cache-update deft-all-files)))
  (deft-filter-update)
  (let ((buf (get-buffer deft-buffer)))
    (when buf
      (with-current-buffer buf
	(deft-buffer-setup)))))

(defun deft-xapian-query-edit ()
  "Enter a Xapian query string, and make it current."
  (interactive)
  (deft-xapian-query-set (deft-xapian-read-query)))

(defun deft-xapian-query-clear ()
  "Clear current Xapian query string."
  (interactive)
  (deft-xapian-query-set nil))

(defun deft-xapian-query-set (new-query)
  "Set NEW-QUERY string as the current Xapian query.
Refresh `deft-all-files' and other state accordingly."
  (unless (equal deft-xapian-query new-query)
    (setq deft-xapian-query new-query)
    (deft-changed 'nothing)))

(defun deft-no-directory-message ()
  "Return an `deft-directory'-does-not-exist message.
That is, return a message to display when the Deft directory
does not exist."
  (concat "Directory " deft-directory " does not exist.\n"))

(defun deft-no-files-message ()
  "Return a short message to display if no files are found."
  (if deft-filter-regexp
      "No files match the current filter string.\n"
    "No files found."))

;; File list file management actions

(defun deft-refresh-after-save ()
  "Refresh Deft state after saving a Deft note file."
  (save-excursion
    (let ((file (buffer-file-name)))
      (deft-changed 'files (list file)))))

;;;###autoload
(defun deft-open-file (file)
  "Open FILE in a new buffer and set its mode.
Set up a hook for refreshing Deft state on save."
  (deft-ensure-init)
  (prog1 (find-file file)
    (add-to-list 'deft-auto-save-buffers (buffer-name))
    (add-hook 'after-save-hook 'deft-refresh-after-save nil t)))

;;;###autoload
(defun deft-save-buffer (pfx)
  "Save the current buffer.
The prefix argument PFX is passed to `save-buffer'.
Set up a hook for refreshing Deft state on save."
  (interactive "P")
  (deft-ensure-init)
  (prog1 (save-buffer pfx)
    (add-to-list 'deft-auto-save-buffers (buffer-name))
    (add-hook 'after-save-hook 'deft-refresh-after-save nil t)))

(defun deft-find-file (file)
  "Find FILE interactively using the minibuffer."
  (interactive "F")
  (deft-open-file file))

(defun deft-sub-new-file (data notename pfx)
  "Create a new file containing the string DATA.
Save into a file with the specified NOTENAME
\(if NOTENAME is nil, generate a name).
With a PFX >= 4, query for a target directory;
otherwise default to `deft-directory'.
With a PFX >= 16, query for a filename extension;
otherwise default to `deft-extension'."
  (let* ((ext (if (and deft-secondary-extensions (>= pfx 16))
		  (deft-read-extension)
		deft-extension))
	 (dir (if (>= pfx 4)
		  (deft-select-directory)
		deft-directory))
	 (file (if notename
		   (deft-make-filename notename ext dir)
		 (deft-generate-filename ext dir))))
    (if (not data)
	(deft-open-file file)
      (write-region data nil file nil nil nil 'excl)
      (deft-changed 'files (list file))
      (deft-open-file file)
      (with-current-buffer (get-file-buffer file)
	(goto-char (point-max))))))

(defun deft-new-file-named (pfx title)
  "Create a new file, prompting for a title.
The prefix argument PFX is as for `deft-new-file'.
Query for a TITLE when invoked as a command."
  (interactive "p\nsNew title: ")
  (if (not (string-match "[a-zA-Z0-9]" title))
      (error "Aborting, unsuitable title: %S" title)
    (deft-sub-new-file title (deft-title-to-notename title) pfx)))

(defun deft-new-file (&optional pfx)
  "Create a new file quickly.
Create it with an automatically generated name, one based
on the `deft-filter-regexp' filter string if it is non-nil.
With a prefix argument PFX, offer a choice of Deft
directories, when `deft-path' has more than one of them.
With two prefix arguments, also offer a choice of filename
extensions when `deft-secondary-extensions' is non-empty."
  (interactive "p")
  (let ((data (and deft-filter-regexp
		   (concat deft-filter-regexp "\n\n")))
	(notename
	 (when deft-filter-regexp
	   (deft-title-to-notename deft-filter-regexp))))
    (deft-sub-new-file data notename pfx)))

(defun deft-file-under-dir-p (dir file)
  "Whether DIR is strictly the parent of FILE."
  (and
   (file-in-directory-p file dir)
   (not (file-equal-p file dir))))

(defun deft-dir-of-deft-file (file)
  "Return the containing `deft-path' directory for FILE.
Return nil if FILE is not under any Deft root."
  (cl-some (lambda (dir)
	     (when (deft-file-under-dir-p dir file)
	       dir))
	   (deft-get-directories)))

(defun deft-direct-file-p (file)
  "Whether FILE is directly in a Deft directory.
More specifically, return non-nil if FILE names
a file or directory that is a direct child of
one of the directories of `deft-path'.
FILE need not actually exist for this predicate to hold."
  (let ((root (deft-dir-of-deft-file file)))
    (and root
	 (file-equal-p file
		       (expand-file-name
			(file-name-nondirectory file)
			root)))))

(defun deft-file-in-subdir-p (file)
  "Whether Deft note FILE is in a sub-directory.
I.e., whether the absolute path FILE names a file or directory
that is in a sub-directory of one of the `deft-path' directories.
FILE need not actually exist for this predicate to hold."
  (let ((root (deft-dir-of-deft-file file)))
    (and root
	 (let ((dir (file-name-directory file)))
	   (not (file-equal-p dir root))))))

(defun deft-is-current-buffer ()
  "Whether the current buffer is a `deft-buffer'."
  (equal (buffer-name (current-buffer)) deft-buffer))

(defun deft-current-filename ()
  "Return the current Deft note filename.
In a `deft-buffer', return the currently selected file's name.
Otherwise return the current buffer's file name, if any.
Otherwise return nil."
  (if (deft-is-current-buffer)
      (widget-get (widget-at) :tag)
    (buffer-file-name)))

(defun deft-no-selected-file-message ()
  (if (deft-is-current-buffer)
      "No file selected"
    "Not in a file buffer"))

;;;###autoload
(defun deft-delete-file (prefix)
  "Delete the selected or current Deft note file.
Prompt before proceeding.
With a prefix argument, also kill the deleted file's buffer, if any."
  (interactive "P")
  (deft-ensure-init)
  (let ((old-file (deft-current-filename)))
    (cond
     ((not old-file)
      (message (deft-no-selected-file-message)))
     (t
      (let ((old-file-nd
	     (file-name-nondirectory old-file)))
	(when (y-or-n-p
	       (concat "Delete file " old-file-nd "? "))
	  (when (file-exists-p old-file)
	    (delete-file old-file))
	  (delq old-file deft-current-files)
	  (delq old-file deft-all-files)
	  (deft-changed 'files (list old-file))
	  (when prefix
	    (let ((buf (get-file-buffer old-file)))
	      (when buf
		(kill-buffer buf))))
	  (message "Deleted %s" old-file-nd)))))))

(defun deft-move-into-subdir (pfx)
  "Move the file at point into a subdirectory of the same name.
To nest more than one level (which is allowed but perhaps atypical),
invoke with a prefix argument PFX."
  (interactive "P")
  (let ((old-file (widget-get (widget-at) :tag)))
    (cond
     ((not old-file)
      (message "Not on a file"))
     ((and (not pfx) (deft-file-in-subdir-p old-file))
      (message "Already in a Deft sub-directory"))
     (t
      (let ((new-file
	     (concat
	      (file-name-directory old-file)
	      (file-name-as-directory (deft-base-filename old-file))
	      (file-name-nondirectory old-file))))
	(deft-rename-file+buffer old-file new-file nil t)
	(deft-changed 'dirs (list (deft-dir-of-deft-file new-file)))
	(message "Renamed as `%s`" new-file))))))

(defun deft-rename-file (pfx)
  "Rename the file represented by the widget at point.
Defaults to a content-derived file name (rather than the old one)
if called with a prefix argument PFX."
  (interactive "P")
  (let ((old-file (widget-get (widget-at) :tag)))
    (cond
     ((not old-file)
      (message "Not on a file"))
     (t
      (let* ((old-name (deft-base-filename old-file))
	     (def-name
	       (or (and
		    pfx
		    (let ((title
			   (deft-title-from-file-content old-file)))
		      (and title
			   (deft-title-to-notename title))))
		   old-name))
	     (new-file
	      (deft-sub-rename-file old-file old-name def-name)))
	(message "Renamed as `%s`" new-file))))))

;;;###autoload
(defun deft-rename-current-file ()
  "Rename current buffer file in a Deft-aware manner.
Query for a new name, using any parsed title to derive
the default name; otherwise default to the old basename."
  (interactive)
  (deft-ensure-init)
  (let ((old-file (buffer-file-name)))
    (when old-file
      (let* ((title (deft-parse-title old-file (buffer-string)))
	     (old-name (deft-base-filename old-file))
	     (def-name (if title
			   (deft-title-to-notename title)
			 old-name)))
	(deft-sub-rename-file old-file old-name def-name)))))

(defun deft-sub-rename-file (old-file old-name def-name)
  "Rename OLD-FILE with the OLD-NAME Deft name.
Query for a new name, defaulting to DEF-NAME.
Use OLD-FILE's filename extension in the new name.
Used by `deft-rename-file' and `deft-rename-current-file'."
  (let* ((history (list def-name))
	 (new-name
	  (read-string
	   (concat "Rename " old-name " to (without extension): ")
	   (car history) ;; INITIAL-INPUT
	   '(history . 1) ;; HISTORY
	   nil ;; DEFAULT-VALUE
	   ))
	 (new-file
	  (deft-make-filename new-name
	    (file-name-extension old-file)
	    (file-name-directory old-file))))
    (deft-rename-file+buffer old-file new-file)
    (when (get-buffer deft-buffer)
      (deft-changed 'dirs (list (deft-dir-of-deft-file new-file))))
    new-file))

(defun deft-rename-file+buffer (old-file new-file &optional exist-ok mkdir)
  "Like `rename-file', rename OLD-FILE as NEW-FILE.
Additionally, rename any OLD-FILE buffer as NEW-FILE,
and also set its visited file as NEW-FILE.
EXIST-OK is as the third argument of `rename-file'.
If MKDIR is non-nil, also create any missing target directory,
but do not create its parent directories."
  (when mkdir
    (ignore-errors
      (make-directory (file-name-directory new-file) nil)))
  (rename-file old-file new-file exist-ok)
  (let ((buf (get-file-buffer old-file)))
    (when buf
      (save-current-buffer
        (set-buffer buf)
        (set-visited-file-name new-file nil t)))))

(defun deft-move-file (old-file new-dir &optional whole-dir)
  "Move the OLD-FILE note file into the NEW-DIR directory.
If OLD-FILE has its own subdirectory, then move the entire
subdirectory, but only if WHOLE-DIR is true.
Return the pathname of the file/directory that was moved."
  (when (and whole-dir (deft-file-in-subdir-p old-file))
    (setq old-file (directory-file-name
		    (file-name-directory old-file))))
  (let ((new-file (concat (file-name-as-directory new-dir)
			  (file-name-nondirectory old-file))))
    (deft-rename-file+buffer old-file new-file)
    old-file))

(defun deft-move-elsewhere (pfx)
  "Move the selected file under selected Deft root.
If it resides in a subdirectory, move the entire
directory, but only if given a prefix argument PFX."
  (interactive "P")
  (let ((old-file (widget-get (widget-at) :tag)))
    (if (not old-file)
	(message "Not on a file")
      (let ((new-root (file-name-as-directory (deft-select-directory)))
	    (old-root (deft-dir-of-deft-file old-file)))
	(unless (file-equal-p new-root old-root)
	  (let ((moved-file (deft-move-file old-file new-root pfx)))
	    (deft-changed 'dirs (list old-root new-root))
	    (message "Moved `%s` under root `%s`" old-file new-root)))))))

(defun deft-archive-file (pfx)
  "Archive the file represented by the widget at point.
If it resides in a subdirectory, archive the entire
directory, but only with a prefix argument PFX."
  (interactive "P")
  (let ((old-file (widget-get (widget-at) :tag)))
    (if (not old-file)
	(message "Not on a file")
      (let ((new-dir
	     (concat (file-name-directory old-file)
		     (file-name-as-directory deft-archive-directory))))
	(let ((moved-file (deft-move-file old-file new-dir pfx)))
	  (deft-changed 'files (list old-file))
	  (message "Archived `%s` into `%s`" old-file new-dir))))))

(defun deft-show-file-info ()
  "Show information about the selected note.
Show filename, title, summary, etc."
  (interactive)
  (let ((file (widget-get (widget-at) :tag)))
    (if (not file)
	(message "Not on a file")
      (let* ((title (deft-file-title file))
	     (summary (deft-file-summary file)))
	(message "name=%S file=%S title=%S summary=%S"
		 (deft-basename-from-file file)
		 file title
		 (and summary
		      (substring summary 0 (min 50 (length summary)))))))))

(defun deft-show-find-file-parse (file)
  "Query for a FILE, and show its parse information."
  (interactive "F")
  (let ((res (with-temp-buffer
	       (insert-file-contents file)
	       (deft-parse-buffer))))
    (message "name=%S file=%S parse=%S"
	     (deft-basename-from-file file)
	     file res)))

(defun deft-show-file-parse ()
  "Show parse information for the file at point."
  (interactive)
  (let ((file (widget-get (widget-at) :tag)))
    (if (not file)
	(message "Not on a file")
      (deft-show-find-file-parse file))))

;; File list filtering

(defun deft-sort-files (files)
  "Sort FILES in reverse order by modification time."
  (sort files (lambda (f1 f2) (deft-file-newer-p f1 f2))))

(defun deft-filter-update ()
  "Update the filtered files list using the current filter regexp.
Refer to `deft-filter-regexp' for the regular expression.
Modify the variable `deft-current-files' to set the result."
  (if (not deft-filter-regexp)
      (setq deft-current-files deft-all-files)
    (setq deft-current-files (mapcar 'deft-filter-match-file deft-all-files))
    (setq deft-current-files (delq nil deft-current-files))))

(defun deft-filter-match-file (file)
  "Return FILE if it passes the current filter regexp."
  (when (string-match-p deft-filter-regexp (deft-file-contents file))
    file))

;; Filters that cause a refresh

(defun deft-filter-clear (&optional pfx)
  "Clear the current filter string and refresh the file browser.
With a prefix argument PFX, also clear any Xapian query."
  (interactive "P")
  (when (or deft-filter-regexp (and pfx deft-xapian-query))
    (setq deft-filter-regexp nil)
    (when pfx
      (setq deft-xapian-query nil))
    (deft-changed 'nothing)))

(defun deft-filter (str)
  "Set the filter string to STR and update the file browser."
  (interactive "sFilter: ")
  (let ((old-regexp deft-filter-regexp))
    (if (string= "" str)
	(setq deft-filter-regexp nil)
      (setq deft-filter-regexp str))
    (unless (equal old-regexp deft-filter-regexp)
      (deft-changed 'nothing))))

(defun deft-filter-increment ()
  "Append character to the filter regexp and update state.
In particular, update `deft-current-files'.
Get the character from the variable `last-command-event'."
  (interactive)
  (let ((char last-command-event))
    (when (= char ?\S-\ )
      (setq char ?\s))
    (setq char (char-to-string char))
    (setq deft-filter-regexp (concat deft-filter-regexp char))
    (deft-changed 'nothing)))

(defun deft-filter-decrement ()
  "Remove last character from the filter regexp and update state.
In particular, update `deft-current-files'."
  (interactive)
  (if (> (length deft-filter-regexp) 1)
      (deft-filter (substring deft-filter-regexp 0 -1))
    (deft-filter-clear)))

(defun deft-complete ()
  "Complete the current action.
If there is a widget at the point, press it.  If a filter is
applied and there is at least one match, open the first matching
file.  If there is an active filter but there are no matches,
quickly create a new file using the filter string as the title.
Otherwise, quickly create a new file."
  (interactive)
  (cond
   ;; Activate widget
   ((widget-at)
    (widget-button-press (point)))
   ;; Active filter string with match
   ((and deft-filter-regexp deft-current-files)
    (deft-open-file (car deft-current-files)))
   ;; Default
   (t
    (deft-new-file))))

;;; Automatic File Saving

(defun deft-auto-save ()
  "Save any modified files in `deft-auto-save-buffers'."
  (save-excursion
    (dolist (buf deft-auto-save-buffers)
      (if (buffer-name buf)
          ;; Save open buffers that have been modified.
          (progn
            (set-buffer buf)
            (when (buffer-modified-p)
              (basic-save-buffer)))
        ;; If a buffer is no longer open, remove it from auto save list.
        (delq buf deft-auto-save-buffers)))))

(defun deft-buffers-gc (kill save)
  "Garbage collect obsolete buffer information.
That is, remove non-existing buffers from `deft-auto-save-buffers'.
Optionally, first KILL unmodified `deft-auto-save-buffers'.
Optionally, SAVE modified buffers before killing any buffers
\(asking for confirmation unless `deft-auto-save-interval' > 0).
Return the buffers removed from `deft-auto-save-buffers'."
  (when save
    (save-some-buffers
     (> deft-auto-save-interval 0)
     (lambda ()
       (member (buffer-name (current-buffer)) deft-auto-save-buffers))))
  (when kill
    (dolist (buf deft-auto-save-buffers)
      (let ((buf (get-buffer buf)))
	(when buf
	  (unless (buffer-modified-p buf)
	    (kill-buffer buf))))))
  (let (dropped)
    (dolist (buf deft-auto-save-buffers)
      (unless (get-buffer buf)
	(setq dropped (cons buf dropped))))
    (dolist (buf dropped dropped)
      (delq buf deft-auto-save-buffers))))

(defun deft-gc (pfx)
  "Garbage collect to remove uncurrent Deft state.
With one prefix argument PFX, also kill unmodified Deft note buffers
\(i.e., all unmodified `deft-auto-save-buffers').
With two prefix arguments, also offer to save any modified buffers."
  (interactive "p")
  (deft-cache-gc)
  (deft-buffers-gc (>= pfx 4) (>= pfx 16)))

;;; Mode definition

(defun deft-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "Deft %s" deft-version))

(defvar deft-mode-map
  (let ((i 0)
        (map (make-keymap)))
    ;; Make multibyte characters extend the filter string.
    (set-char-table-range (nth 1 map) (cons #x100 (max-char))
                          'deft-filter-increment)
    ;; Extend the filter string by default.
    (setq i ?\s)
    (while (< i 256)
      (define-key map (vector i) 'deft-filter-increment)
      (setq i (1+ i)))
    ;; Handle backspace and delete
    (define-key map (kbd "DEL") 'deft-filter-decrement)
    ;; Handle return via completion or opening file
    (define-key map (kbd "RET") 'deft-complete)
    ;; Filtering
    (define-key map (kbd "C-c C-l") 'deft-filter)
    (define-key map (kbd "C-c C-c") 'deft-filter-clear)
    ;; File creation
    (define-key map (kbd "C-c C-n") 'deft-new-file)
    (define-key map (kbd "C-c C-m") 'deft-new-file-named)
    ;; File management
    (define-key map (kbd "C-c i") 'deft-show-file-info)
    (define-key map (kbd "C-c p") 'deft-show-file-parse)
    (define-key map (kbd "C-c P") 'deft-show-find-file-parse)
    (define-key map (kbd "C-c C-d") 'deft-delete-file)
    (define-key map (kbd "C-c C-r") 'deft-rename-file)
    (define-key map (kbd "C-c C-f") 'deft-find-file)
    (define-key map (kbd "C-c C-b") 'deft-move-into-subdir)
    (define-key map (kbd "C-c C-a") 'deft-archive-file)
    (define-key map (kbd "C-c x m") 'deft-move-elsewhere)
    ;; Miscellaneous
    (define-key map (kbd "C-c C-j") 'deft-chdir)
    (define-key map (kbd "C-c g") 'deft-refresh)
    (define-key map (kbd "C-c G") 'deft-gc)
    (define-key map (kbd "C-c C-q") 'quit-window)
    ;; Widgets
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map [down-mouse-2] 'widget-button-click)
    ;; Xapian
    (when deft-xapian-program
      (define-key map (kbd "C-c r") 'deft-xapian-re-index)
      (define-key map (kbd "<tab>") 'deft-xapian-query-edit)
      (define-key map (kbd "<backtab>") 'deft-xapian-query-clear)
      (define-key map (kbd "<S-tab>") 'deft-xapian-query-clear))
    map)
  "Keymap for Deft mode.")

;;;###autoload
(defun deft-refresh (prefix)
  "Refresh or reset Deft state.
Refresh Deft state so that filesystem changes get noticed.
With a PREFIX argument, reset state, so that caches and
queries and such are also cleared.
Invoke this command manually if Deft files change outside of
`deft-mode', as such changes are not detected automatically."
  (interactive "P")
  (if prefix
      (deft-ensure-init t)
    (deft-ensure-init)
    (setq deft-directories (deft-resolve-directories))
    (deft-changed 'anything)))

(defun deft-file-member (file list)
  "Whether FILE is a member of LIST."
  (and (cl-some (lambda (x) (file-equal-p file x)) list) t))

(defun deft-ensure-init (&optional always dirs dir)
  "Initialize Deft state unless already initialized.
If ALWAYS is non-nil, initialize unconditionally.
The optional argument DIRS specifies the Deft directories to use.
The optional argument DIR specifies the initial `deft-directory'
to set, or a function for determining it from among DIRS."
  (when (or always (not deft-hash-mtimes))
    (deft-cache-initialize)
    (setq deft-filter-regexp nil)
    (setq deft-xapian-query nil)
    (setq deft-directories (or dirs (deft-resolve-directories)))
    (when (or always (not deft-directory))
      (let ((dir (cond
		  ((stringp dir) dir)
		  ((functionp dir) (funcall dir dirs))
		  ((and deft-directory
			(deft-file-member deft-directory deft-directories))
		   deft-directory)
		  (t (deft-select-directory dirs)))))
	(setq deft-directory (file-name-as-directory (expand-file-name dir)))))
    (deft-changed 'anything)))

(defun deft-mode ()
  "Major mode for quickly browsing, filtering, and editing plain text notes.
Turning on `deft-mode' runs the hook `deft-mode-hook'.

\\{deft-mode-map}."
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (use-local-map deft-mode-map)
  (setq major-mode 'deft-mode)
  (setq mode-name "Deft")
  (when (> deft-auto-save-interval 0)
    (run-with-idle-timer deft-auto-save-interval t 'deft-auto-save))
  (run-mode-hooks 'deft-mode-hook))

(put 'deft-mode 'mode-class 'special)

(defun deft-create-buffer (&optional dirs dir)
  "Create and switch to a `deft-mode' buffer.
If a Deft buffer already exists, its state is reset.
The DIRS and DIR arguments are as for `deft-ensure-init'."
  (deft-ensure-init t dirs dir)
  (switch-to-buffer deft-buffer)
  (deft-mode)
  (deft-buffer-setup)
  (message "Using Deft data directory '%s'" deft-directory))

;;;###autoload
(defun deft (&optional pfx)
  "Switch to `deft-buffer', creating it if not yet created.
With a prefix argument PFX, always query for an initial
`deft-directory' choice for a newly created Deft buffer,
and otherwise query only as necessary."
  (interactive "P")
  (let ((buf (get-buffer deft-buffer)))
    (cond
     (buf
      (switch-to-buffer buf))
     ((and deft-directory (not pfx) (file-exists-p deft-directory))
      (deft-create-buffer))
     (t
      (let ((roots (deft-resolve-directories)))
	(cond
	 (pfx
	  (deft-create-buffer roots 'deft-select-directory))
	 ((null roots)
	  (error "Empty `deft-path'"))
	 ((= 1 (length roots))
	  (let ((dir (car roots)))
	    (unless (file-exists-p dir)
	      (make-directory dir t))
	    (deft-create-buffer roots dir)))
	 (deft-xapian-program
	   (let ((dir (cl-some (lambda (dir)
				 (when (file-exists-p dir)
				   dir))
			       roots)))
	     (if dir
		 (deft-create-buffer roots dir)
	       (error "No existing directory on `deft-path'"))))
	 (t
	  (deft-create-buffer roots 'deft-select-directory))))))))

(defun deft-filter-existing-dirs (in-lst)
  "Pick existing directories in IN-LST.
That is, filter the argument list, rejecting anything
except for names of existing directories."
  (let (lst)
    (mapc (lambda (d)
	    (when (file-directory-p d)
	      (setq lst (cons d lst))))
	  (reverse in-lst))
    lst))

(defun drop-nth-cons (n lst)
  "Make list element at position N the first one of LST.
That is, functionally move that element to position 0."
  (let* ((len (length lst))
	 (rst (- len n)))
    (cons (nth n lst) (append (butlast lst rst) (last lst (- rst 1))))))

;;;###autoload
(defun deft-read-extension ()
  "Read a Deft filename extension, interactively.
The default choice is `deft-extension', but any of the
`deft-secondary-extensions' are also available as choices."
  (ido-completing-read
   "Extension: "
   (cons deft-extension deft-secondary-extensions)
   nil t))

;;;###autoload
(defun deft-select-directory (&optional dirs)
  "Select a Deft directory, possibly interactively.
Select from the configured list of directories (i.e., `deft-path');
any DIRS argument overrides the configured list of choices.
Non-existing directories are not available for selecting.
If `default-directory' is a Deft one, use that as the default choice.
Return the selected directory, or error out."
  (let ((roots (or dirs (deft-get-directories))))
    (if (not roots)
	(error "No specified Deft data directories")
      (let ((lst (deft-filter-existing-dirs roots)))
	(if (not lst)
	    (error "No existing Deft data directories")
	  (if (= (length lst) 1)
	      (car lst)
	    (let* ((ix
		    (cl-position-if
		     (lambda (x) (file-equal-p default-directory x))
		     lst))
		   (choice-lst
		    (if ix (drop-nth-cons ix lst) lst))
		   (d (ido-completing-read
		       "Data directory: " choice-lst
		       nil 'confirm-after-completion
		       nil nil nil t)))
	      (if (not d)
		  (error "Nothing selected")
		(if (not (file-directory-p d))
		    (error "Not a directory")
		  d)))))))))

;;;###autoload
(defun deft-chdir ()
  "Change `deft-directory' according to interactive selection."
  (interactive)
  (if (not deft-hash-mtimes)
      (deft-ensure-init t nil 'deft-select-directory)
    (deft-ensure-init nil)
    (let ((dir (deft-select-directory)))
      (setq deft-directory (file-name-as-directory (expand-file-name dir)))
      (unless deft-xapian-program
	(deft-changed 'anything)))))

;;;###autoload
(defun deft-open-file-by-basename (filename)
  "Open a Deft file named FILENAME.
FILENAME is a non-directory filename, with an extension
\(it is not necessarily unique)."
  (deft-ensure-init)
  (let ((fn (deft-file-by-basename filename)))
    (if (not fn)
	(message "No Deft note '%s'" filename)
      (deft-open-file fn))))

;;;###autoload
(defun deft-open-query ()
  "Open Deft with an interactively read Xapian search query.
Create a `deft-buffer' if one does not yet exist,
otherwise merely switch to the existing Deft buffer."
  (interactive)
  (when deft-xapian-program
    (let ((query (deft-xapian-read-query)))
      (deft)
      (deft-xapian-query-set query))))

;;;###autoload
(defun deft-lucky-find-file ()
  "Open the highest-ranked note matching a search query.
Read the query interactively, accounting for `deft-xapian-query-history'.
Open the file directly, without switching to any `deft-buffer'."
  (interactive)
  (when deft-xapian-program
    (deft-ensure-init)
    (let* ((query (deft-xapian-read-query))
	   (deft-xapian-order-by-time nil)
	   (deft-xapian-max-results 1)
	   (files (deft-xapian-search (deft-get-directories) query)))
      (if (not files)
	  (message "No matching notes found")
	(deft-open-file (car files))))))

(provide 'deft)

;;; deft.el ends here
