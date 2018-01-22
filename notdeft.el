;;; notdeft.el --- quickly browse, filter, and edit plain text notes
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

;; NotDeft is an Emacs mode for quickly browsing, filtering, and
;; editing directories of plain text notes. It was designed for
;; increased productivity when writing and taking notes by making it
;; fast to find the right file at the right time.

;; NotDeft is open source software and may be freely distributed and
;; modified under the BSD license.  This version is a fork of
;; Deft version 0.3, which was released on September 11, 2011.

;; File Browser

;; The NotDeft buffer is simply a file browser which lists the titles of
;; all text files in the NotDeft directory (or directories) followed by
;; short summaries and last modified times. The title is taken to be
;; the first line of the file (or as specified by an Org "TITLE" file
;; property) and the summary is extracted from the text that follows.
;; Files are sorted in terms of the last modified date, from newest to
;; oldest.

;; All NotDeft files or notes are simple plain text files (or Org markup
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

;; Filtering

;; NotDeft's primary operation is searching and filtering.  The list of
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

;; Common file operations can also be carried out from within NotDeft.
;; Files can be renamed using `C-c C-r` or deleted using `C-c C-d`.
;; New files can also be created using `C-c C-n` for quick creation or
;; `C-c C-m` for a filename prompt.  You can leave NotDeft at any time
;; with `C-c C-q`.

;; Archiving unused files can be carried out by pressing `C-c C-a`.
;; Files will be moved to `notdeft-archive-directory' under a NotDeft
;; data directory (e.g., current `notdeft-directory').

;; Getting Started
;; ---------------

;; To start using it, place it somewhere in your Emacs load-path and
;; add the line

;;     (require 'notdeft-autoloads)

;; in your `.emacs` file.  Then run `M-x notdeft` to start.  It is useful
;; to create a global keybinding for the `notdeft` function (e.g., a
;; function key) to start it quickly (see below for details).

;; One useful way to use NotDeft is to keep a directory of notes in a
;; Dropbox folder.  This can be used with other applications and
;; mobile devices, for example, Notational Velocity or Simplenote
;; on OS X, Elements on iOS, or Epistle on Android.

;; Customization
;; -------------

;; Customize the `notdeft` group to change the functionality.

;; By default, NotDeft looks for notes by searching for files with the
;; extension `.org` in the `~/.deft` directory.  You can customize
;; both the file extension and the NotDeft note search path by running
;; `M-x customize-group` and typing `notdeft`.  Alternatively, you can
;; configure them in your `.emacs` file:

;;     (setq notdeft-directories '("~/.deft/" "~/Dropbox/notes/"))
;;     (setq notdeft-extension "txt")
;;     (setq notdeft-secondary-extensions '("md" "scrbl"))

;; The variable `notdeft-extension' specifies the default extension
;; for new notes. There can be `notdeft-secondary-extensions' for
;; files that are also considered to be NotDeft notes.

;; While you can choose a `notdeft-extension' that is not ".org",
;; this fork of NotDeft is somewhat optimized to working with
;; files in Org format.

;; You can easily set up a global keyboard binding for NotDeft.  For
;; example, to bind it to F8, add the following code to your `.emacs`
;; file:

;;     (global-set-key [f8] 'notdeft)

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

;; NotDeft version 0.3:

;; * Most notably, add a Xapian-based query engine.
;; * Add support for multiple notes directories.

;; Deft version 0.3 (2011-09-11):

;; * Internationalization: support filtering with multibyte characters.

;; Deft version 0.2 (2011-08-22):

;; * Match filenames when filtering.
;; * Automatically save opened files (optional).
;; * Address some byte-compilation warnings.

;; Deft was originally written by Jason Blevins.
;; The initial version, 0.1, was released on August 6, 2011.

;;; Code:

(require 'cl-lib)
(require 'widget)
(require 'wid-edit)

;; Customization

;;;###autoload
(defgroup notdeft nil
  "Emacs NotDeft mode."
  :group 'local)

(defcustom notdeft-directories '("~/.deft/")
  "NotDeft directories.
Each element must be a directory path string.
Each named directory may or may not exist."
  :type '(repeat string)
  :safe (lambda (lst) (cl-every 'stringp lst))
  :group 'notdeft)

(defcustom notdeft-directory nil
  "Default or previously selected NotDeft data directory.
One of the `notdeft-directories', or nil if none."
  :type '(choice (string :tag "Default directory")
		 (const :tag "None" nil))
  :safe 'string-or-null-p
  :group 'notdeft)

(defcustom notdeft-extension "org"
  "Default NotDeft file extension."
  :type 'string
  :safe 'stringp
  :group 'notdeft)

(defcustom notdeft-secondary-extensions nil
  "Additional NotDeft file extensions."
  :type '(repeat string)
  :safe (lambda (lst) (cl-every 'stringp lst))
  :group 'notdeft)

(defcustom notdeft-notename-function 'notdeft-default-title-to-notename
  "Function for deriving a note name from a title.
Returns nil if no name can be derived from the argument."
  :type 'function
  :group 'notdeft)

(defcustom notdeft-archive-directory "_archive"
  "Sub-directory name for archived notes.
Should begin with '.', '_', or '#' to be excluded from
indexing for Xapian searches."
  :type 'string
  :safe 'stringp
  :group 'notdeft)

(defcustom notdeft-time-format " %Y-%m-%d %H:%M"
  "Format string for modification times in the NotDeft browser.
Set to nil to hide."
  :type '(choice (string :tag "Time format")
		 (const :tag "Hide" nil))
  :safe 'string-or-null-p
  :group 'notdeft)

(defcustom notdeft-file-display-function nil
  "Formatter for file names in the NotDeft browser.
If a function, it must accept the filename and
a maximum width as its two arguments.
Set to nil to hide."
  :type '(choice (function :tag "Formatting function")
		 (const :tag "Hide" nil))
  :safe 'null
  :group 'notdeft)

;; Faces

(defgroup notdeft-faces nil
  "Faces used in NotDeft mode"
  :group 'notdeft
  :group 'faces)

(defface notdeft-header-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for NotDeft header."
  :group 'notdeft-faces)

(defface notdeft-filter-string-face
  '((t :inherit font-lock-string-face))
  "Face for NotDeft filter string."
  :group 'notdeft-faces)

(defface notdeft-title-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for NotDeft file titles."
  :group 'notdeft-faces)

(defface notdeft-separator-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Face for NotDeft separator string."
  :group 'notdeft-faces)

(defface notdeft-summary-face
  '((t :inherit font-lock-comment-face))
  "Face for NotDeft file summary strings."
  :group 'notdeft-faces)

(defface notdeft-time-face
  '((t :inherit font-lock-variable-name-face))
  "Face for NotDeft last modified times."
  :group 'notdeft-faces)

;; Internal requires

(require 'notdeft-global)
(require 'notdeft-xapian)

;; Constants

(defconst notdeft-buffer "*NotDeft*"
  "NotDeft buffer name.")

(defconst notdeft-separator " --- "
  "Text used to separate file titles and summaries.")

;; Global variables

(defvar notdeft-load-hook nil
  "Hook run immediately after `notdeft' feature load.")

(defvar notdeft-mode-hook nil
  "Hook run when entering NotDeft mode.")

(defvar notdeft-pre-refresh-hook nil
  "Hook run before each `notdeft-refresh'.")

(defvar notdeft-post-refresh-hook nil
  "Hook run after each `notdeft-refresh'.")

(defvar notdeft-xapian-query nil
  "Current Xapian query string.
Where `notdeft-xapian-program' is available, it determines the
contents of `notdeft-all-files' for a NotDeft buffer.")

(defvar notdeft-filter-string nil
  "Current filter string used by NotDeft.
It is treated as a list of whitespace-separated strings (not
regular expressions) that are required to match.")

(defvar notdeft-all-files nil
  "List of all files to list or filter.")

(defvar notdeft-current-files nil
  "List of files matching current filter.")

(defvar notdeft-hash-contents (make-hash-table :test 'equal)
  "Hash containing complete cached file contents, keyed by filename.")

(defvar notdeft-hash-mtimes (make-hash-table :test 'equal)
  "Hash containing cached file modification times, keyed by filename.")

(defvar notdeft-hash-titles (make-hash-table :test 'equal)
  "Hash containing cached file titles, keyed by filename.")

(defvar notdeft-hash-summaries (make-hash-table :test 'equal)
  "Hash containing cached file summaries, keyed by filename.")

(defvar notdeft-window-width nil
  "Width of NotDeft buffer, as currently drawn, or nil.")

(defvar notdeft-pending-updates 'rescan
  "Whether there are pending updates for any NotDeft buffer.
Either nil for no pending updates, the symbol `redraw' for a
pending redrawing of the buffer, the symbol `recompute' for a
pending recomputation of `notdeft-current-files', the symbol
`rescan' for a pending rescanning of `notdeft-all-files'.")

;; File processing

(defun notdeft-title-to-notename (str)
  "Call `notdeft-notename-function' on STR."
  (funcall notdeft-notename-function str))

(defun notdeft-default-title-to-notename (str)
  "Turn a title string STR to a note name string.
Return that string, or nil if no usable name can be derived."
  (when (string-match "^[^a-zA-Z0-9-]+" str)
    (setq str (replace-match "" t t str)))
  (when (string-match "[^a-zA-Z0-9-]+$" str)
    (setq str (replace-match "" t t str)))
  (while (string-match "[`'“”\"]" str)
    (setq str (replace-match "" t t str)))
  (while (string-match "[^a-zA-Z0-9-]+" str)
    (setq str (replace-match "-" t t str)))
  (setq str (downcase str))
  (and (not (string= "" str)) str))

(defun notdeft-format-time-for-filename (tm)
  "Format time TM suitably for filenames."
  (format-time-string "%Y-%m-%d-%H-%M-%S" tm t)) ; UTC

(defun notdeft-generate-notename (&optional fmt)
  "Generate a notename, and return it.
The generated name is not guaranteed to be unique. Format with
the format string FMT, or \"Deft--%s\" otherwise."
  (let* ((ctime (current-time))
	 (ctime-s (notdeft-format-time-for-filename ctime))
	 (base-filename (format (or fmt "Deft--%s") ctime-s)))
    base-filename))

(defun notdeft-generate-filename (&optional ext dir fmt)
  "Generate a new unique filename.
Do so without being given any information about note title or
content. Have the file have the extension EXT, and be in
directory DIR \(their defaults are as for `notdeft-make-filename').
Pass FMT to `notdeft-generate-notename'."
  (let (filename)
    (while (or (not filename)
	       (file-exists-p filename))
      (let ((base-filename (notdeft-generate-notename fmt)))
	(setq filename (notdeft-make-filename base-filename ext dir))))
    filename))

(defun notdeft-make-filename (notename &optional ext dir in-subdir)
  "Derive a filename from NotDeft note name NOTENAME.
The filename shall have the extension EXT,
defaulting to `notdeft-extension'.
The file shall reside in the directory DIR (or a default directory
computed by `notdeft-get-directory'), except that IN-SUBDIR indicates
that the file should be given its own subdirectory."
  (let ((root (or dir (notdeft-get-directory))))
    (concat (file-name-as-directory root)
	    (if in-subdir (file-name-as-directory notename) "")
	    notename "." (or ext notdeft-extension))))

(defun notdeft-make-file-re ()
  "Return a regexp matching strings with a NotDeft extension."
  (let ((exts (cons notdeft-extension notdeft-secondary-extensions)))
    (concat "\\.\\(?:"
	    (mapconcat 'regexp-quote exts "\\|")
	    "\\)$")))

(defun notdeft-strip-extension (file)
  "Strip any NotDeft filename extension from FILE."
  (replace-regexp-in-string (notdeft-make-file-re) "" file))
  
(defun notdeft-base-filename (file)
  "Strip the leading path and NotDeft extension from filename FILE.
Use `file-name-directory' to get the directory component.
Strip any extension with `notdeft-strip-extension'."
  (let* ((file (file-name-nondirectory file))
	 (file (notdeft-strip-extension file)))
    file))

(defun notdeft-basename-from-file (file)
  "Extract the basename of the note FILE."
  (file-name-nondirectory file))

(defun notdeft-file-readable-p (file)
  "Whether FILE is a readable non-directory."
  (and (file-readable-p file)
       (not (file-directory-p file))))

(defun notdeft-read-file (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;;###autoload
(defun notdeft-title-from-file-content (file)
  "Extract a title from FILE content.
Return nil on failure."
  (when (notdeft-file-readable-p file)
    (let* ((contents (notdeft-read-file file))
	   (title (notdeft-parse-title file contents)))
      title)))

(defun notdeft-chomp (str)
  "Trim leading and trailing whitespace from STR."
  (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)"
			    "" str))

;;;###autoload
(defun notdeft-file-by-basename (name)
  "Resolve a NotDeft note NAME to a full pathname.
NAME is a non-directory filename, with extension.
Resolve it to the path of a file under `notdeft-directories',
if such a note file does exist.
If multiple such files exist, return one of them.
If none exist, return nil."
  (let* ((file-p (lambda (pn)
		   (string= name (file-name-nondirectory pn))))
	 (cand-roots notdeft-directories)
	 result)
    (while (and cand-roots (not result))
      (let ((abs-root (expand-file-name (car cand-roots))))
	(setq cand-roots (cdr cand-roots))
	(setq result (notdeft-root-find-file file-p abs-root))))
    result))

(defun notdeft-root-find-file (file-p abs-dir)
  "Find a file matching predicate FILE-P under ABS-DIR.
ABS-DIR is assumed to be a NotDeft root.
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
	   (setq result (notdeft-root-find-file file-p abs-file)))
	  ((funcall file-p abs-file)
	   (setq result abs-file)))))
     result)))

(defun notdeft-glob (root &optional dir result file-re)
  "Return a list of all NotDeft files in a directory tree.
List the NotDeft files under the specified NotDeft ROOT and
its directory DIR, with DIR given as a path relative
to the directory ROOT.
If DIR is nil, then list NotDeft files under ROOT.
Add to the RESULT list in an undefined order,
and return the resulting value.
Only include files matching regexp FILE-RE, defaulting
to the result of `notdeft-make-file-re'."
  (let* ((root (file-name-as-directory (expand-file-name root)))
	 (dir (file-name-as-directory (or dir ".")))
	 (abs-dir (expand-file-name dir root)))
    (and
     (file-readable-p abs-dir)
     (file-directory-p abs-dir)
     (let* ((files (directory-files abs-dir nil "^[^._#]" t))
	    (file-re (or file-re (notdeft-make-file-re))))
       (dolist (file files result)
	 (let* ((rel-file (file-relative-name
			   (expand-file-name file abs-dir)
			   root))
		(abs-file (concat root rel-file)))
	   (cond
	    ((file-directory-p abs-file)
	     (setq result (notdeft-glob root rel-file result file-re)))
	    ((string-match-p file-re file)
	     (setq result (cons rel-file result))))))))))

(defun notdeft-glob/absolute (root &optional dir result file-re)
  "Like `notdeft-glob', but return the results as absolute paths.
The arguments ROOT, DIR, RESULT, and FILE-RE are the same."
  (mapcar
   (lambda (rel)
     (expand-file-name rel root))
   (notdeft-glob root dir result file-re)))

(defun notdeft-find-all-files-in-dir (dir full)
  "Return a list of all NotDeft files under DIR.
The specified directory must be a NotDeft root.
Return an empty list if there is no readable directory.
Return the files' absolute paths if FULL is true."
  (if full
      (notdeft-glob/absolute dir)
    (notdeft-glob dir)))

;;;###autoload
(defun notdeft-make-basename-list ()
  "Return the names of all NotDeft notes.
Search all existing `notdeft-directories'.
The result list is sorted by the `string-lessp' relation.
It may contain duplicates."
  (let ((dir-lst notdeft-directories)
	(fn-lst '()))
    (dolist (dir dir-lst)
      (setq fn-lst
	    (append fn-lst
		    (notdeft-find-all-files-in-dir dir t))))
    ;; `sort` may modify `name-lst`
    (let ((name-lst (mapcar 'notdeft-basename-from-file fn-lst)))
      (sort name-lst 'string-lessp))))

(defun notdeft-parse-title (file contents)
  "Parse the given FILE CONTENTS and determine the title.
The title is taken to be the first non-empty line of a file.
Org comments are skipped, and \"#+TITLE\" syntax is recognized,
and may also be used to define the title.
Returns nil if there is no non-empty, not-just-whitespace
title in CONTENTS."
  (let* ((res (with-temp-buffer
		(insert contents)
		(notdeft-parse-buffer)))
	 (title (car res)))
    title))

(defun notdeft-substring-from (str from max-n)
  "Extract a substring from STR.
Extract it from position FROM, and up to MAX-N characters."
  (substring str from (max (length str) (+ from max-n))))

(defun notdeft-condense-whitespace (str)
  "Condense whitespace in STR into a single space."
  (replace-regexp-in-string "[[:space:]\n]+" " " str))

;;;###autoload
(defun notdeft-chomp-nullify (str &optional trim)
  "Return string STR if non-empty, otherwise return nil.
Optionally, use function TRIM to trim any result string."
  (when str
    (let ((str (notdeft-chomp str)))
      (unless (string= "" str)
	(if trim (funcall trim str) str)))))

(defun notdeft-parse-buffer ()
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
     (notdeft-chomp-nullify title)
     (notdeft-chomp-nullify summary 'notdeft-condense-whitespace)
     (notdeft-chomp-nullify keywords)
     dbg)))

(defun notdeft-cache-initialize ()
  "Initialize hash tables for caching files."
  (setq notdeft-hash-contents (make-hash-table :test 'equal))
  (setq notdeft-hash-mtimes (make-hash-table :test 'equal))
  (setq notdeft-hash-titles (make-hash-table :test 'equal))
  (setq notdeft-hash-summaries (make-hash-table :test 'equal)))

(defun notdeft-cache-clear ()
  "Clear the cache of file information."
  (clrhash notdeft-hash-mtimes)
  (clrhash notdeft-hash-titles)
  (clrhash notdeft-hash-summaries)
  (clrhash notdeft-hash-contents))

(defun notdeft-cache-remove-file (file)
  "Remove FILE from the cache.
Do nothing if FILE is not in the cache."
  (remhash file notdeft-hash-mtimes)
  (remhash file notdeft-hash-titles)
  (remhash file notdeft-hash-summaries)
  (remhash file notdeft-hash-contents))

(defun notdeft-cache-gc ()
  "Remove obsolete entries from the cache.
That is, remove information for files that no longer exist.
Return a list of the files whose information was removed."
  (let (lst)
    (maphash (lambda (file v)
	       (unless (file-exists-p file)
		 (setq lst (cons file lst))))
	     notdeft-hash-mtimes)
    (dolist (file lst lst)
      (notdeft-cache-remove-file file))))

(defun notdeft-cache-file (file)
  "Update file cache for FILE.
Keep any information for a non-existing file."
  (when (file-exists-p file)
    (let ((mtime-cache (notdeft-file-mtime file))
          (mtime-file (nth 5 (file-attributes file))))
      (when (or (not mtime-cache)
		(time-less-p mtime-cache mtime-file))
	(notdeft-cache-newer-file file mtime-file)))))

(defun notdeft-cache-newer-file (file mtime)
  "Update cached information for FILE with given MTIME."
  (let* ((res (with-temp-buffer
		(insert-file-contents file)
		(notdeft-parse-buffer)))
	 (title (car res))
	 (summary (cadr res))
	 (contents
	  (concat file " "
		  (or title "") " "
		  (or (car (cddr res)) "") " "
		  (or summary ""))))
    (puthash file mtime notdeft-hash-mtimes)
    (puthash file title notdeft-hash-titles)
    (puthash file summary notdeft-hash-summaries)
    (puthash file contents notdeft-hash-contents)))

(defun notdeft-file-newer-p (file1 file2)
  "Return non-nil if FILE1 was modified since FILE2 and nil otherwise."
  (let (time1 time2)
    (setq time1 (notdeft-file-mtime file1))
    (setq time2 (notdeft-file-mtime file2))
    (time-less-p time2 time1)))

;; Cache access

(defun notdeft-file-contents (file)
  "Retrieve complete contents of FILE from cache."
  (gethash file notdeft-hash-contents))

(defun notdeft-file-mtime (file)
  "Retrieve modified time of FILE from cache."
  (gethash file notdeft-hash-mtimes))

(defun notdeft-file-title (file)
  "Retrieve title of FILE from cache."
  (gethash file notdeft-hash-titles))

(defun notdeft-file-summary (file)
  "Retrieve summary of FILE from cache."
  (gethash file notdeft-hash-summaries))

;; File list display

(defun notdeft-print-header ()
  "Prints the *NotDeft* buffer header."
  (widget-insert
   (propertize "NotDeft: " 'face 'notdeft-header-face))
  (when notdeft-xapian-query
    (widget-insert
     (propertize (concat notdeft-xapian-query ": ")
		 'face 'notdeft-xapian-query-face)))
  (when notdeft-filter-string
    (widget-insert
     (propertize notdeft-filter-string 'face 'notdeft-filter-string-face)))
  (widget-insert "\n\n"))

(eval-when-compile
  (defvar notdeft-mode-map))

(defun notdeft-buffer-setup ()
  "Render the NotDeft file browser in the current buffer."
  (let ((line (max 3 (line-number-at-pos))))
    (setq notdeft-window-width (window-width))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (notdeft-print-header)

    ;; Print the files list
    (if (not notdeft-directories)
	"No NotDeft data directories.\n"
      (if notdeft-current-files
	  (mapc 'notdeft-file-widget notdeft-current-files) ;; for side effects
	(widget-insert
	 (if notdeft-filter-string
	     "No files match the current filter string.\n"
	   "No files found.\n"))))

    (widget-setup)
    
    (goto-char (point-min))
    (forward-line (1- line))))

(defun notdeft-file-widget (file)
  "Add a line to the file browser for the given FILE."
  (let* ((text (notdeft-file-contents file))
	 (title (notdeft-file-title file))
	 (summary (notdeft-file-summary file))
	 (mtime (when notdeft-time-format
		  (format-time-string notdeft-time-format
				      (notdeft-file-mtime file))))
	 (line-width (- notdeft-window-width (length mtime)))
	 (path (when notdeft-file-display-function
		 (funcall notdeft-file-display-function file line-width)))
	 (path-width (length path))
	 (up-to-path-width (- line-width path-width))
	 (title-width (min up-to-path-width (length title)))
	 (summary-width (min (length summary)
			     (- up-to-path-width
				title-width
				(length notdeft-separator)))))
    (widget-create 'link
		   :button-prefix ""
		   :button-suffix ""
		   :button-face 'notdeft-title-face
		   :format "%[%v%]"
		   :tag file
		   :help-echo "Edit this file"
		   :notify (lambda (widget &rest ignore)
			     (notdeft-find-file (widget-get widget :tag)))
		   (if title
		       (substring title 0 title-width)
		     "[Empty file]"))
    (when (> summary-width 0)
      (widget-insert (propertize notdeft-separator 'face 'notdeft-separator-face))
      (widget-insert (propertize
		      (if summary (substring summary 0 summary-width) "")
		      'face 'notdeft-summary-face)))
    (when (or path mtime)
      (while (< (current-column) up-to-path-width)
	(widget-insert " ")))
    (when path
      (widget-insert (propertize path 'face 'notdeft-time-face)))
    (when mtime
      (widget-insert (propertize mtime 'face 'notdeft-time-face)))
    (widget-insert "\n")))

(defun notdeft-map-drop-false (function sequence &optional no-order)
  "Like `mapcar' of FUNCTION and SEQUENCE, but filtering nils.
Optionally, if NO-ORDER is true, return the results without
retaining order."
  (let (lst)
    (dolist (elt sequence)
      (let ((elt (funcall function elt)))
	(when elt
	  (setq lst (cons elt lst)))))
    (if no-order lst (reverse lst))))

(defun notdeft-files-under-all-roots ()
  "Return a list of all NotDeft files under `notdeft-directories'.
Return the results as absolute paths, in any order."
  (let (result
	(file-re (notdeft-make-file-re)))
    (dolist (dir notdeft-directories result)
      (setq result (notdeft-glob/absolute dir nil result file-re)))))

(defun notdeft-cache-update (files)
  "Update cached information for FILES."
  (mapc 'notdeft-cache-file files))

(defun notdeft-xapian-index-files (files)
  "Update Xapian index for FILES (at least)."
  (let ((dirs
	 (delete-dups
	  (mapcar 'notdeft-dir-of-file files))))
    (notdeft-xapian-index-dirs dirs)))

(defun notdeft-xapian-re-index ()
  "Recreate all Xapian indexes in `notdeft-directories'."
  (interactive)
  (when notdeft-xapian-program
    (notdeft-xapian-index-dirs notdeft-directories t)
    (notdeft-changed/query)))

(defmacro notdeft-if2 (cnd thn els)
  "Two-armed `if'.
Equivalent to (if CND THN ELS)."
  (declare (indent defun))
  `(if ,cnd ,thn ,els))

(defmacro notdeft-setq-cons (x v)
  "Prepend into list X the value V."
  `(setq ,x (cons ,v ,x)))

(defmacro notdeft-setq-non-nil (x v)
  "To X, assign the result of deleting nils from list V."
  `(setq ,x (delete nil ,v)))

(defun notdeft-pending-lessp (x y)
  "Whether pending status value X < Y."
  (let ((lst '(() redraw recompute rescan)))
    (< (cl-position x lst) (cl-position y lst))))

(defun notdeft-set-pending-updates (value)
  "Set `notdeft-pending-updates' to at least VALUE."
  (when (notdeft-pending-lessp notdeft-pending-updates value)
    (setq notdeft-pending-updates value)))

(defun notdeft-changed/fs (what &optional things)
  "Refresh NotDeft file list, cache, and search index state.
The arguments hint at what may need refreshing.

WHAT is a symbolic hint for purposes of optimization.
It is one of:
- symbol `dirs' to assume changes in THINGS NotDeft directories;
- symbol `files' to assume changes in THINGS NotDeft files; or
- symbol `anything' to make no assumptions about filesystem changes.

Ignore THINGS outside NotDeft directory trees.

As appropriate, refresh both file information cache and
any Xapian indexes, and update `notdeft-all-files' and
`notdeft-current-files' lists to reflect those changes,
or changes to `notdeft-filter-string' or `notdeft-xapian-query'."
  (let (dirs files) ;; filtered to Deft ones
    (cl-case what
      (dirs
       (notdeft-setq-non-nil
	dirs (mapcar 'notdeft-directories-member things)))
      (files
       (dolist (file things)
	 (let ((dir (notdeft-dir-of-file file)))
	   (when dir
	     (notdeft-setq-cons files file)
	     (notdeft-setq-cons dirs dir))))))
    (if (or (and (eq what 'files) (not files))
	    (and (eq what 'dirs) (not dirs)))
	(progn) ;; no filesystem change
      (notdeft-if2
       notdeft-xapian-program
       (progn
	 (cl-case what
	   (anything (notdeft-xapian-index-dirs notdeft-directories))
	   ((dirs files) (notdeft-xapian-index-dirs (delete-dups dirs))))
	 (setq notdeft-all-files
	       (notdeft-xapian-search notdeft-directories
				      notdeft-xapian-query))
	 (notdeft-cache-update notdeft-all-files))
       (progn
	 (cl-case what
	   (files
	    (notdeft-cache-update files)
	    (dolist (file files)
	      (setq notdeft-all-files (delete file notdeft-all-files))
	      (when (file-exists-p file)
		(setq notdeft-all-files (cons file notdeft-all-files)))))
	   (t
	    (setq notdeft-all-files (notdeft-files-under-all-roots))
	    (notdeft-cache-update notdeft-all-files)))
	 (setq notdeft-all-files (notdeft-sort-files notdeft-all-files))))
      (setq notdeft-pending-updates 'recompute)
      (notdeft-changed/window))))

(defun notdeft-rescan-all-files ()
  "Fully recompute `notdeft-all-files'.
Also update file information cache.
Do nothing else."
  (notdeft-if2 notdeft-xapian-program
    (progn
      (setq notdeft-all-files
	    (notdeft-xapian-search notdeft-directories
				   notdeft-xapian-query))
      (notdeft-cache-update notdeft-all-files))
    (let ((files (notdeft-files-under-all-roots)))
      (notdeft-cache-update files)
      (setq notdeft-all-files (notdeft-sort-files files)))))

(defun notdeft-changed/query ()
  "Refresh NotDeft buffer after query change."
  (when notdeft-xapian-program
    (setq notdeft-all-files
	  (notdeft-xapian-search notdeft-directories notdeft-xapian-query))
    (notdeft-cache-update notdeft-all-files)
    (notdeft-changed/filter)))

(defun notdeft-changed/filter ()
  "Refresh NotDeft buffer after filter change."
  (notdeft-set-pending-updates 'recompute)
  (notdeft-changed/window))

(defun notdeft-changed/window ()
  "Perform any pending operations on a buffer.
Only do that if a `notdeft-buffer' is visible.
Update `notdeft-pending-updates' accordingly."
  (when notdeft-pending-updates
    (let ((buf (get-buffer notdeft-buffer)))
      (when (and buf (get-buffer-window buf 'visible))
	(cond
	 ((eq notdeft-pending-updates 'rescan)
	  (notdeft-rescan-all-files)
	  (notdeft-filter-update))
	 ((eq notdeft-pending-updates 'recompute)
	  (notdeft-filter-update)))
	(with-current-buffer buf
	  (notdeft-buffer-setup))
	(setq notdeft-pending-updates nil)))))

(defun notdeft-window-configuration-changed ()
  "A `window-configuration-change-hook' for NotDeft.
Called with the change event concerning the `selected-window',
whose current buffer should be a NotDeft buffer, as the hook
is installed locally for NotDeft buffers only."
  (unless (equal notdeft-window-width (window-width))
    (unless notdeft-pending-updates
      (notdeft-set-pending-updates 'redraw)))
  (notdeft-changed/window))

(defun notdeft-query-edit ()
  "Enter a Xapian query string, and make it current."
  (interactive)
  (notdeft-xapian-query-set (notdeft-xapian-read-query)))

(defun notdeft-query-clear ()
  "Clear current Xapian query string."
  (interactive)
  (notdeft-xapian-query-set nil))

(defun notdeft-xapian-query-set (new-query)
  "Set NEW-QUERY string as the current Xapian query.
Refresh `notdeft-all-files' and other state accordingly, as
`notdeft-changed/query' does it. Additionally, display a message
summarizing some statistics about the results shown."
  (setq notdeft-xapian-query new-query)
  (notdeft-changed/query)
  (let* ((n (length notdeft-all-files))
	 (is-none (= n 0))
	 (is-max (and (> notdeft-xapian-max-results 0)
		      (= n notdeft-xapian-max-results)))
	 (found (cond
		 (is-max (format "Found maximum of %d notes" n))
		 (is-none "Found no notes")
		 (t (format "Found %d notes" n))))
	 (shown (cond
		 (is-none "")
		 (notdeft-filter-string
		  (format ", showing %d of them"
			  (length notdeft-current-files)))
		 (t ", showing all of them"))))
    (message (concat found shown))))

;; File list file management actions

;;;###autoload
(define-minor-mode notdeft-note-mode
  "Manage NotDeft state for a note buffer.
A minor mode that is enabled automatically for notes opened from
within a `notdeft-mode' buffer. Does nothing but manage calls to
`notdeft-register-buffer' and `notdeft-deregister-buffer'."
  :lighter " ¬D"
  (if notdeft-note-mode
      (notdeft-register-buffer)
    (notdeft-deregister-buffer)))
  
(defun notdeft-refresh-after-save ()
  "Refresh global NotDeft state after saving a NotDeft note."
  (let ((file (buffer-file-name)))
    (when file
      (notdeft-changed/fs 'files (list file)))))

(defun notdeft-register-buffer (&optional buffer)
  "Register BUFFER for saving as a NotDeft note.
Use `current-buffer' as the default buffer.
Ensure that global NotDeft state gets refreshed on save."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (add-hook 'after-save-hook 'notdeft-refresh-after-save nil t))))

(defun notdeft-deregister-buffer (&optional buffer)
  "Deregister a NotDeft BUFFER.
Use `current-buffer' as the default buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (remove-hook 'after-save-hook 'notdeft-refresh-after-save t))))

;;;###autoload
(defun notdeft-register-file (file)
  "Enable NotDeft note mode for any buffer of FILE."
  (let ((buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf
	(notdeft-note-mode 1)))))

;;;###autoload
(defun notdeft-save-buffer (prefix)
  "Save the current buffer as a NotDeft note.
Enable NotDeft note minor mode before saving.
The PREFIX argument is passed to `save-buffer'."
  (interactive "P")
  (notdeft-note-mode 1)
  (save-buffer prefix))

(defun notdeft-note-mode-buffers ()
  "Return a list of NotDeft note buffers.
The list contains references to buffers with for which the
NotDeft note minor mode has been enabled, and thus the variable
`notdeft-note-mode' is bound and set."
  (notdeft-map-drop-false 'notdeft-note-buffer-p (buffer-list) t))

;;;###autoload
(defun notdeft-switch-to-buffer ()
  "Switch to an existing NotDeft note buffer.
The list of choices is determined by the function
`notdeft-note-mode-buffers'."
  (interactive)
  (let ((buffers (notdeft-note-mode-buffers)))
    (cond
     ((not buffers)
      (message "No NotDeft notes open"))
     ((null (cdr buffers))
      (switch-to-buffer (car buffers)))
     (t
      (let* ((names (mapcar 'buffer-name buffers))
	     (name (ido-completing-read "Buffer: " names nil t)))
	(switch-to-buffer name))))))
		     
;;;###autoload
(defun notdeft-find-file (file)
  "Edit NotDeft note FILE.
Enable NotDeft note mode for the buffer for editing the file.
Called interactively, query for the FILE using the minibuffer."
  (interactive "FFind NotDeft file: ")
  (prog1 (find-file file)
    (notdeft-note-mode 1)))

;;;###autoload
(defun notdeft-create-file (&optional dir notename ext data)
  "Create a new NotDeft note file.
Create it into the directory DIR with basename NOTENAME and
filename extension EXT, and write any DATA into the file. If any
of those values are nil, then use a default value. If DIR or EXT
is the symbol `ask', then query the user for a directory or
extension. If DIR is a non-empty list, then offer the user that
choice list of directories. If NOTENAME is of the form (format
FMT), then use `notdeft-generate-filename' to generate a filename
with the format string FMT. If NOTENAME is of the form (title
STR), then use `notdeft-title-to-notename' to generate a notename
from STR."
  (let* ((dir (pcase dir
	       ((pred stringp)
		dir)
	       ((pred consp)
		(notdeft-select-directory dir "Directory for new file: "))
	       (`ask
		(notdeft-select-directory nil "Directory for new file: "))
	       (_
		(notdeft-get-directory))))
	 (ext (pcase ext
	       ((pred stringp)
		ext)
	       (`ask
		(notdeft-read-extension))
	       (_
		notdeft-extension)))
	 (file (pcase notename
		((pred stringp)
		 (notdeft-make-filename notename ext dir))
		((and `(title ,(and (pred stringp) title))
		      (let name (notdeft-title-to-notename title))
		      (guard name))
		 (notdeft-make-filename name ext dir))
		(`(format ,(and (pred stringp) fmt))
		 (notdeft-generate-filename ext dir fmt))
		(_
		 (notdeft-generate-filename ext dir)))))
    (if (not data)
	(notdeft-find-file file)
      (write-region data nil file nil nil nil 'excl)
      (notdeft-changed/fs 'files (list file))
      (notdeft-find-file file)
      (with-current-buffer (get-file-buffer file)
	(goto-char (point-max))))
    file))

(defun notdeft-sub-new-file (&optional data notename pfx)
  "Create a new file containing the string DATA.
Save into a file with the specified NOTENAME
\(if NOTENAME is nil, generate a name).
With a PFX >= '(4), query for a target directory;
otherwise default to the result of `notdeft-get-directory'.
With a PFX >= '(16), query for a filename extension;
otherwise default to `notdeft-extension'.
Return the name of the new file."
  (let ((pfx (prefix-numeric-value pfx)))
    (notdeft-create-file
      (and (>= pfx 4) 'ask)
      notename
      (and (>= pfx 16) 'ask)
      data)))

;;;###autoload
(defun notdeft-switch-to-file-named (title &optional data)
  "Switch to a NotDeft note with the specified TITLE.
It is assumed that a notename has been derived from
the title with `notdeft-title-to-notename'.
If no note so named exists, create one.
Initialize any created file with DATA, or TITLE if not given."
  (let ((notename (notdeft-title-to-notename title)))
    (unless notename
      (error "Aborting, unsuitable title: %S" title))
    (let* ((basename (concat notename "." notdeft-extension))
	   (file (notdeft-file-by-basename basename)))
      (if (not file)
	  (notdeft-sub-new-file (or data title) notename)
	(notdeft-find-file file)
	file))))

;;;###autoload
(defun notdeft-new-file-named (pfx title &optional data)
  "Create a new file, prompting for a title.
The prefix argument PFX is as for `notdeft-new-file'.
Query for a TITLE when invoked as a command.
Initialize the file with DATA, or TITLE if not given.
Return the filename of the created file."
  (interactive "P\nsNew title: ")
  (let ((notename (notdeft-title-to-notename title)))
    (notdeft-sub-new-file (or data title) notename pfx)))

;;;###autoload
(defun notdeft-new-file (pfx)
  "Create a new file quickly.
Create it with an automatically generated name, one based
on the `notdeft-filter-string' filter string if it is non-nil.
With a prefix argument PFX, offer a choice of NotDeft
directories, when there is more than one of them.
With two prefix arguments, also offer a choice of filename
extensions when `notdeft-secondary-extensions' is non-empty.
Return the filename of the created file."
  (interactive "P")
  (let ((data (and notdeft-filter-string
		   (concat notdeft-filter-string "\n\n")))
	(notename
	 (and notdeft-filter-string
	      (notdeft-title-to-notename notdeft-filter-string))))
    (notdeft-sub-new-file data notename pfx)))

(defun notdeft-dir-of-file (file)
  "Return NotDeft root of FILE, or nil.
FILE may also itself be one of the `notdeft-directories'."
  (cl-some (lambda (dir)
	     (when (file-in-directory-p file dir)
	       dir))
	   notdeft-directories))

(defun notdeft-file-under-dir-p (dir file)
  "Whether DIR is strictly the parent of FILE."
  (and
   (file-in-directory-p file dir)
   (not (file-equal-p file dir))))

(defun notdeft-dir-of-notdeft-file (file)
  "Return the containing NotDeft directory for FILE.
Return nil if FILE is not strictly under some NotDeft root."
  (cl-some (lambda (dir)
	     (when (notdeft-file-under-dir-p dir file)
	       dir))
	   notdeft-directories))

(defun notdeft-direct-file-p (file)
  "Whether FILE is directly in a NotDeft directory.
More specifically, return non-nil if FILE names
a file or directory that is a direct child of
one of the `notdeft-directories'.
FILE need not actually exist for this predicate to hold."
  (let ((root (notdeft-dir-of-notdeft-file file)))
    (and root
	 (file-equal-p file
		       (expand-file-name
			(file-name-nondirectory file)
			root)))))

(defun notdeft-file-in-subdir-p (file)
  "Whether NotDeft note FILE is in a sub-directory.
I.e., whether the absolute path FILE names a file or directory
that is in a sub-directory of one of the `notdeft-directories'.
FILE need not actually exist for this predicate to hold."
  (let ((root (notdeft-dir-of-notdeft-file file)))
    (and root
	 (let ((dir (file-name-directory file)))
	   (not (file-equal-p dir root))))))

(defun notdeft-file-member (file list)
  "Whether FILE is a member of LIST.
Return the matching member of the list, or nil."
  (cl-some (lambda (elem)
	     (when (file-equal-p file elem)
	       elem))
	   list))

(defun notdeft-directories-member (file)
  "Whether FILE is a NotDeft directory.
Return the matching member of `notdeft-directories'."
  (notdeft-file-member file notdeft-directories))

(defun notdeft-note-buffer-p (&optional buffer)
  "Whether BUFFER is a NotDeft Note mode buffer.
Default to `current-buffer' if BUFFER is nil.
Return the buffer, or nil."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (when notdeft-note-mode
	buffer))))

(defun notdeft-buffer-p (&optional buffer)
  "Whether BUFFER is a `notdeft-mode' buffer.
Default to `current-buffer' if BUFFER is nil.
Return the buffer, or nil."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (when (eq major-mode 'notdeft-mode)
	buffer))))

(defun notdeft-get-directory ()
  "Select a NotDeft directory for an operation.
If in a NotDeft note buffer, and `default-directory' or one of
its parents is a NotDeft directory, then use that directory.
Otherwise, use any previously selected `notdeft-directory'. All
else failing, query using `notdeft-select-directory', and cache
the result into `notdeft-directory'."
  (or (when (notdeft-note-buffer-p)
	(cl-some
	 (lambda (root)
	   (when (file-in-directory-p default-directory root)
	     root))
	 notdeft-directories))
      notdeft-directory
      (let ((dir (notdeft-select-directory)))
	(setq notdeft-directory (file-name-as-directory dir))
	dir)))
	 
(defun notdeft-current-filename ()
  "Return the current NotDeft note filename.
In a `notdeft-mode' buffer, return the currently selected file's
name. Otherwise return the current buffer's file name, if any.
Otherwise return nil."
  (if (notdeft-buffer-p)
      (widget-get (widget-at) :tag)
    (buffer-file-name)))

(defun notdeft-no-selected-file-message ()
  "Return a \"file not selected\" message."
  (if (notdeft-buffer-p)
      "No file selected"
    "Not in a file buffer"))

(defun notdeft-select-file ()
  "Open the selected file, if any."
  (interactive)
  (let ((old-file (notdeft-current-filename)))
    (if (not old-file)
	(message (notdeft-no-selected-file-message))
      (notdeft-find-file old-file))))

;;;###autoload
(defun notdeft-delete-file (prefix)
  "Delete the selected or current NotDeft note file.
Prompt before proceeding.
With a PREFIX argument, also kill the deleted file's buffer, if any."
  (interactive "P")
  (let ((old-file (notdeft-current-filename)))
    (cond
     ((not old-file)
      (message (notdeft-no-selected-file-message)))
     (t
      (let ((old-file-nd
	     (file-name-nondirectory old-file)))
	(when (y-or-n-p
	       (concat "Delete file " old-file-nd "? "))
	  (when (file-exists-p old-file)
	    (delete-file old-file))
	  (delq old-file notdeft-current-files)
	  (delq old-file notdeft-all-files)
	  (notdeft-changed/fs 'files (list old-file))
	  (when prefix
	    (let ((buf (get-file-buffer old-file)))
	      (when buf
		(kill-buffer buf))))
	  (message "Deleted %s" old-file-nd)))))))

;;;###autoload
(defun notdeft-move-into-subdir (pfx)
  "Move the file at point into a subdirectory of the same name.
To nest more than one level (which is allowed but perhaps atypical),
invoke with a prefix argument PFX."
  (interactive "P")
  (let ((old-file (notdeft-current-filename)))
    (cond
     ((not old-file)
      (message (notdeft-no-selected-file-message)))
     ((and (not pfx) (notdeft-file-in-subdir-p old-file))
      (message "Already in a NotDeft sub-directory"))
     (t
      (let ((new-file
	     (concat
	      (file-name-directory old-file)
	      (file-name-as-directory (notdeft-base-filename old-file))
	      (file-name-nondirectory old-file))))
	(notdeft-rename-file+buffer old-file new-file nil t)
	(notdeft-changed/fs 'dirs
	  (list (notdeft-dir-of-notdeft-file new-file)))
	(message "Renamed as `%s`" new-file))))))

;;;###autoload
(defun notdeft-change-file-extension ()
  "Change the filename extension of a NotDeft note.
Operate on the selected or current NotDeft note file."
  (interactive)
  (let ((old-file (notdeft-current-filename)))
    (cond
     ((not notdeft-secondary-extensions)
      (message "Only one configured extension"))
     ((not old-file)
      (message (notdeft-no-selected-file-message)))
     (t
      (let* ((old-ext (file-name-extension old-file))
	     (new-ext (notdeft-read-extension old-ext)))
	(unless (string= old-ext new-ext)
	  (let ((new-file (concat (file-name-sans-extension old-file)
				  "." new-ext)))
	    (notdeft-rename-file+buffer old-file new-file)
	    (when (get-buffer notdeft-buffer)
	      (notdeft-changed/fs
	       'dirs
	       (list (notdeft-dir-of-notdeft-file new-file))))
	    (message "Renamed as `%s`" new-file))))))))

;;;###autoload
(defun notdeft-rename-file (pfx)
  "Rename the selected or current NotDeft note file.
Defaults to a content-derived file name (rather than the old one)
if called with a prefix argument PFX."
  (interactive "P")
  (let ((old-file (notdeft-current-filename)))
    (cond
     ((not old-file)
      (message (notdeft-no-selected-file-message)))
     (t
      (let* ((old-name (notdeft-base-filename old-file))
	     (def-name
	       (or (when pfx
		     (let ((title
			    (if (notdeft-buffer-p)
				(notdeft-title-from-file-content old-file)
			      (notdeft-parse-title old-file (buffer-string)))))
		       (and title (notdeft-title-to-notename title))))
		   old-name))
	     (new-file (notdeft-sub-rename-file old-file old-name def-name)))
	(message "Renamed as `%s`" new-file))))))

(defun notdeft-sub-rename-file (old-file old-name def-name)
  "Rename OLD-FILE with the OLD-NAME NotDeft name.
Query for a new name, defaulting to DEF-NAME.
Use OLD-FILE's filename extension in the new name.
Used by `notdeft-rename-file' and `notdeft-rename-current-file'."
  (let* ((history (list def-name))
	 (new-name
	  (read-string
	   (concat "Rename " old-name " to (without extension): ")
	   (car history) ;; INITIAL-INPUT
	   '(history . 1) ;; HISTORY
	   nil ;; DEFAULT-VALUE
	   ))
	 (new-file
	  (notdeft-make-filename new-name
	    (file-name-extension old-file)
	    (file-name-directory old-file))))
    (unless (string= old-file new-file)
      (notdeft-rename-file+buffer old-file new-file)
      (when (get-buffer notdeft-buffer)
	(notdeft-changed/fs
	 'dirs
	 (list (notdeft-dir-of-notdeft-file new-file)))))
    new-file))

(defun notdeft-rename-file+buffer (old-file new-file &optional exist-ok mkdir)
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

(defun notdeft-sub-move-file (old-file new-dir &optional whole-dir)
  "Move the OLD-FILE note file into the NEW-DIR directory.
If OLD-FILE has its own subdirectory, then move the entire
subdirectory, but only if WHOLE-DIR is true.
Return the pathname of the file/directory that was moved."
  (when (notdeft-file-in-subdir-p old-file)
    (unless whole-dir
      (error "Attempt to move file in a sub-directory: %s" old-file))
    (setq old-file (directory-file-name
		    (file-name-directory old-file))))
  (let ((new-file (concat (file-name-as-directory new-dir)
			  (file-name-nondirectory old-file))))
    (notdeft-rename-file+buffer old-file new-file)
    old-file))

(defvar notdeft-previous-target nil
  "Previous file move target NotDeft directory.
Set to nil if `notdeft-move-file' has not been used to move a
file.")

;;;###autoload
(defun notdeft-move-file (pfx)
  "Move the selected file under selected NotDeft root.
If it resides in a subdirectory, move the entire directory, but
only if given a prefix argument PFX. Moving an external
\(non-Deft) file under a NotDeft root is also allowed."
  (interactive "P")
  (let ((old-file (notdeft-current-filename)))
    (if (not old-file)
	(message (notdeft-no-selected-file-message))
      (let* ((old-root (notdeft-dir-of-notdeft-file old-file))
	     (choices ;; exclude any `old-root'
	      (if (not old-root)
		  notdeft-directories
		(cl-remove-if (lambda (dir)
				(file-equal-p dir old-root))
			      notdeft-directories)))
	     (choices ;; default to any `notdeft-previous-target'
	      (if (not notdeft-previous-target)
		  choices
		(notdeft-list-prefer
		 choices
		 (lambda (dir)
		   (file-equal-p dir notdeft-previous-target)))))
	     (new-root
	      (file-name-as-directory
	       (notdeft-select-directory choices nil t t))))
	(when (or (not old-root)
		  (not (file-equal-p new-root old-root)))
	  (let ((moved-file (notdeft-sub-move-file old-file new-root pfx)))
	    (setq notdeft-previous-target new-root)
	    (notdeft-changed/fs
	     'dirs (delete nil (list old-root new-root)))
	    (message "Moved `%s` under root `%s`" old-file new-root)))))))

;;;###autoload
(defun notdeft-archive-file (pfx)
  "Archive the selected NotDeft note file.
Archive it under `notdeft-archive-directory', under its NotDeft
root directory. If it resides in a subdirectory, archive the
entire directory, but only with a prefix argument PFX."
  (interactive "P")
  (let ((old-file (notdeft-current-filename)))
    (if (not old-file)
	(message (notdeft-no-selected-file-message))
      (let ((new-dir
	     (concat (file-name-directory old-file)
		     (file-name-as-directory notdeft-archive-directory))))
	(let ((moved-file (notdeft-sub-move-file old-file new-dir pfx)))
	  (notdeft-changed/fs 'files (list old-file))
	  (message "Archived `%s` into `%s`" old-file new-dir))))))

(eval-when-compile
  (defvar deft-directory))
(declare-function deft-refresh "deft")

;;;###autoload
(defun notdeft-open-in-deft ()
  "Open the selected note's Deft directory in Deft.
Do that only when the command `deft' is available. This
implementation makes assumptions about Deft."
  (interactive)
  (when (fboundp 'deft)
    (let ((old-file (notdeft-current-filename)))
      (if (not old-file)
	  (message (notdeft-no-selected-file-message))
	(let ((old-dir (notdeft-dir-of-notdeft-file old-file)))
	  (if (not old-dir)
	      (message "Not a NotDeft file: %s" old-file)
	    (let ((re-init
		   (and (boundp 'deft-buffer)
			(get-buffer deft-buffer)
			(not (equal deft-directory old-dir)))))
	      (setq deft-directory old-dir)
	      (deft)
	      (when re-init
		(deft-refresh)))))))))

;;;###autoload
(defun notdeft-show-file-directory ()
  "Show NotDeft directory of the selected note."
  (interactive)
  (let ((old-file (notdeft-current-filename)))
    (if (not old-file)
	(message (notdeft-no-selected-file-message))
      (let ((dir (notdeft-dir-of-notdeft-file old-file)))
	(if (not dir)
	    (message "Not on a NotDeft file")
	  (message "%s" dir))))))

(defun notdeft-show-file-info ()
  "Show information about the selected note.
Show filename, title, summary, etc."
  (interactive)
  (let ((file (widget-get (widget-at) :tag)))
    (if (not file)
	(message "Not on a file")
      (let* ((title (notdeft-file-title file))
	     (summary (notdeft-file-summary file)))
	(message "name=%S file=%S title=%S summary=%S"
		 (notdeft-basename-from-file file)
		 file title
		 (and summary
		      (substring summary 0 (min 50 (length summary)))))))))

(defun notdeft-show-find-file-parse (file)
  "Query for a FILE, and show its parse information."
  (interactive "F")
  (let ((res (with-temp-buffer
	       (insert-file-contents file)
	       (notdeft-parse-buffer))))
    (message "name=%S file=%S parse=%S"
	     (notdeft-basename-from-file file)
	     file res)))

(defun notdeft-show-file-parse ()
  "Show parse information for the file at point."
  (interactive)
  (let ((file (widget-get (widget-at) :tag)))
    (if (not file)
	(message "Not on a file")
      (notdeft-show-find-file-parse file))))

;; File list filtering

(defun notdeft-sort-files (files)
  "Sort FILES in reverse order by modification time."
  (sort files (lambda (f1 f2) (notdeft-file-newer-p f1 f2))))

(defun notdeft-filter-update ()
  "Update the filtered files list using the current filter string.
Refer to `notdeft-filter-string' for the string.
Modify the variable `notdeft-current-files' to set the result."
  (if (not notdeft-filter-string)
      (setq notdeft-current-files notdeft-all-files)
    (setq notdeft-current-files
	  (mapcar 'notdeft-filter-match-file notdeft-all-files))
    (setq notdeft-current-files (delq nil notdeft-current-files))))

(defun notdeft-filter-match-file (file)
  "Return FILE if it is a match against the current filter string.
Treat `notdeft-filter-string' as a list of whitespace-separated
strings and require all elements to match."
  (let ((contents (notdeft-file-contents file))
	(filter-lst
	 (mapcar 'regexp-quote (split-string notdeft-filter-string))))
    (when (cl-every (lambda (filter)
		      (string-match-p filter contents))
		    filter-lst)
      file)))
  
;; Filters that cause a refresh

(defun notdeft-filter-clear (&optional pfx)
  "Clear the current filter string and refresh the file browser.
With a prefix argument PFX, also clear any Xapian query."
  (interactive "P")
  (cond
   ((and pfx notdeft-xapian-query)
    (setq notdeft-xapian-query nil)
    (setq notdeft-filter-string nil)
    (notdeft-changed/query))
   (notdeft-filter-string
    (setq notdeft-filter-string nil)
    (notdeft-changed/filter))))

(defun notdeft-filter (str)
  "Set the filter string to STR and update the file browser."
  (interactive "sFilter: ")
  (let ((old-filter notdeft-filter-string))
    (setq notdeft-filter-string (and (not (equal "" str)) str))
    (unless (equal old-filter notdeft-filter-string)
      (notdeft-changed/filter))))

(defun notdeft-filter-increment ()
  "Append character to the filter string and update state.
In particular, update `notdeft-current-files'.
Get the character from the variable `last-command-event'."
  (interactive)
  (let ((char last-command-event))
    (when (= char ?\S-\ )
      (setq char ?\s))
    (setq char (char-to-string char))
    (setq notdeft-filter-string (concat notdeft-filter-string char))
    (notdeft-changed/filter)))

(defun notdeft-filter-decrement ()
  "Remove last character from the filter string and update state.
In particular, update `notdeft-current-files'."
  (interactive)
  (if (> (length notdeft-filter-string) 1)
      (notdeft-filter (substring notdeft-filter-string 0 -1))
    (notdeft-filter-clear)))

(defun notdeft-filter-decrement-word ()
  "Remove last word from the filter, if possible, and update.
This is like `backward-kill-word' on the filter string, but the
kill ring is not affected."
  (interactive)
  (when notdeft-filter-string
    (let ((new-filter
	   (with-temp-buffer
	     (insert notdeft-filter-string)
	     (goto-char (point-max))
	     (backward-word)
	     (buffer-substring-no-properties (point-min) (point)))))
      (notdeft-filter new-filter))))

(defun notdeft-filter-yank ()
  "Append the most recently killed or yanked text to the filter."
  (interactive)
  (let ((s (current-kill 0 t)))
    (notdeft-filter
     (if notdeft-filter-string
	 (concat notdeft-filter-string s)
       s))))

(defun notdeft-complete ()
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
   ((and notdeft-filter-string notdeft-current-files)
    (notdeft-find-file (car notdeft-current-files)))
   ;; Default
   (t
    (notdeft-new-file 1))))

(defun notdeft-gc ()
  "Garbage collect to remove uncurrent NotDeft state.
More specifically, delete obsolete cached file information."
  (interactive)
  (notdeft-cache-gc))

;;; Mode definition

(defvar notdeft-mode-map
  (let ((i 0)
        (map (make-keymap)))
    ;; Make multibyte characters extend the filter string.
    (set-char-table-range (nth 1 map) (cons #x100 (max-char))
                          'notdeft-filter-increment)
    ;; Extend the filter string by default.
    (setq i ?\s)
    (while (< i 256)
      (define-key map (vector i) 'notdeft-filter-increment)
      (setq i (1+ i)))
    ;; Handle return via completion or opening file
    (define-key map (kbd "RET") 'notdeft-complete)
    ;; Filtering
    (define-key map (kbd "DEL") 'notdeft-filter-decrement)
    (define-key map (kbd "C-c C-l") 'notdeft-filter)
    (define-key map (kbd "C-c C-c") 'notdeft-filter-clear)
    (define-key map (kbd "C-y") 'notdeft-filter-yank)
    (define-key map (kbd "M-DEL") 'notdeft-filter-decrement-word)
    (define-key map (kbd "<C-S-backspace>") 'notdeft-filter-clear)
    ;; File management
    (define-key map (kbd "C-c I") 'notdeft-show-file-info)
    (define-key map (kbd "C-c p") 'notdeft-show-file-parse)
    (define-key map (kbd "C-c P") 'notdeft-show-find-file-parse)
    ;; Miscellaneous
    (define-key map (kbd "C-c b") 'notdeft-switch-to-buffer)
    (define-key map (kbd "C-c G") 'notdeft-gc)
    (define-key map (kbd "C-c C-q") 'quit-window)
    ;; Widgets
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map [down-mouse-2] 'widget-button-click)
    ;; Xapian
    (when notdeft-xapian-program
      (define-key map (kbd "C-c R") 'notdeft-xapian-re-index)
      (define-key map (kbd "<tab>") 'notdeft-query-edit)
      (define-key map (kbd "<backtab>") 'notdeft-query-clear)
      (define-key map (kbd "<S-tab>") 'notdeft-query-clear))
    (let ((parent-map (make-sparse-keymap)))
      (define-key parent-map (kbd "C-c") 'notdeft-global-map)
      (set-keymap-parent map parent-map)
      map))
  "Keymap for NotDeft mode.")

(defun notdeft-reset ()
  "Reset NotDeft state.
Clear some of the state. This includes the file information
cache, search query, and filter string."
  (notdeft-cache-clear)
  (setq notdeft-xapian-query nil)
  (setq notdeft-filter-string nil)
  (setq notdeft-pending-updates 'rescan))

;;;###autoload
(defun notdeft-refresh (&optional reset)
  "Refresh or reset NotDeft state.
Refresh NotDeft state so that outside filesystem changes get
noticed. With a non-nil prefix argument RESET, also reset state
to clear caches and queries and such. Invoke this command
manually if NotDeft files change outside of NotDeft mode and
NotDeft note minor mode \(as toggled by the command
`notdeft-mode' and the command `notdeft-note-mode'), as such
changes are not detected automatically."
  (interactive "P")
  (run-hooks 'notdeft-pre-refresh-hook)
  (when reset
    (notdeft-reset))
  (notdeft-changed/fs 'anything)
  (run-hooks 'notdeft-post-refresh-hook))

(defun notdeft-mode ()
  "Major mode for quickly listing and managing plain text notes.
Turning on `notdeft-mode' runs the hook `notdeft-mode-hook'. Only
run this function when a buffer named `notdeft-buffer' is
current.

\\{notdeft-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (use-local-map notdeft-mode-map)
  (setq major-mode 'notdeft-mode)
  (setq mode-name "NotDeft")
  (add-hook 'window-configuration-change-hook ;; buffer locally
	    'notdeft-window-configuration-changed nil t)
  (run-mode-hooks 'notdeft-mode-hook))

(put 'notdeft-mode 'mode-class 'special)

(defun notdeft-create-buffer ()
  "Create and switch to a `notdeft-mode' buffer.
If a NotDeft buffer already exists, its state is reset."
  (switch-to-buffer notdeft-buffer)
  (notdeft-mode)
  (notdeft-changed/window)
  (when notdeft-directory
    (message "Using NotDeft data directory '%s'" notdeft-directory)))

(defun notdeft-maybe-offer-create-directory ()
  "Maybe offer to create a NotDeft directory.
If no `notdeft-directories' exist, offer to create one of them.
Return any created directory, or nil."
  (when notdeft-directories
    (let ((roots (notdeft-filter-existing-dirs notdeft-directories)))
      (unless roots
	(let ((root (car notdeft-directories)))
	  (if (file-exists-p root)
	      (error "Data \"directory\" is a non-directory: %s" root)
	    (when (y-or-n-p (concat "Create directory " root "? "))
	      (make-directory root t)
	      root)))))))

;;;###autoload
(defun notdeft (&optional reset)
  "Switch to `notdeft-buffer', creating it if not yet created.
With a non-nil prefix argument RESET, start NotDeft with fresh
state. With two prefix arguments, also interactively query for an
initial choice of `notdeft-directory'."
  (interactive "P")
  (when (equal reset '(16))
    (setq notdeft-directory
	  (file-name-as-directory (notdeft-select-directory))))
  (when reset
    (notdeft-reset))
  (let ((buf (get-buffer notdeft-buffer)))
    (if buf
	(progn
	  (switch-to-buffer buf)
	  (when reset
	    (notdeft-changed/window)))
      (notdeft-maybe-offer-create-directory)
      (notdeft-set-pending-updates 'redraw)
      (notdeft-create-buffer))))

(defun notdeft-filter-existing-dirs (lst)
  "Pick existing directories in LST.
That is, filter the argument list, rejecting anything
except for names of existing directories."
  (notdeft-map-drop-false
    (lambda (d)
      (when (file-directory-p d)
	d))
    lst))

(defun drop-nth-cons (n lst)
  "Make list element at position N the first one of LST.
That is, functionally move that element to position 0."
  (let* ((len (length lst))
	 (rst (- len n)))
    (cons (nth n lst) (append (butlast lst rst) (last lst (- rst 1))))))

;;;###autoload
(defun notdeft-read-extension (&optional prefer)
  "Read a NotDeft filename extension, interactively.
The default choice is `notdeft-extension', but any of the
`notdeft-secondary-extensions' are also available as choices.
With a PREFER argument, use that extension as the first choice."
  (if (not notdeft-secondary-extensions)
      notdeft-extension
    (let* ((choices (cons notdeft-extension notdeft-secondary-extensions))
	   (choices (if prefer
			(notdeft-list-prefer choices
			  `(lambda (ext) (string= ,prefer ext)))
		      choices)))
      (ido-completing-read "Extension: " choices nil t))))

(defun notdeft-list-prefer (choices prefer)
  "Re-order the CHOICES list to make preferred element first.
PREFER is a predicate for identifying such an element.
Move only the first matching element, if any.
Return CHOICES as is if there are no matching elements."
  (let ((ix (cl-position-if prefer choices)))
    (if ix (drop-nth-cons ix choices) choices)))

;;;###autoload
(defun notdeft-select-directory (&optional dirs prompt confirm preserve)
  "Select a NotDeft directory, possibly interactively.
If DIRS is non-nil, select from among those directories;
otherwise select from `notdeft-directories'. Use the specified
PROMPT in querying, if given. Return the selected directory, or
error out. If CONFIRM is non-nil, query even if there is a single
choice. Present any `notdeft-directory' as the first choice,
except with a true PRESERVE argument, which preserves DIRS
order."
  (let ((dirs (or dirs notdeft-directories)))
    (if (not dirs)
	(error "No specified NotDeft data directories")
      (let ((dirs (notdeft-filter-existing-dirs dirs)))
	(cond
	 ((not dirs)
	  (error "No existing NotDeft data directories"))
	 ((and (not confirm) (= (length dirs) 1))
	  (car dirs))
	 (t
	  (when (and notdeft-directory (not preserve))
	    (setq dirs (notdeft-list-prefer
		       dirs
		       (lambda (file)
			 (file-equal-p notdeft-directory file)))))
	  (ido-completing-read (or prompt "Data directory: ")
			       dirs nil t)))))))

;;;###autoload
(defun notdeft-chdir ()
  "Change `notdeft-directory' according to interactive selection.
Query for a directory with `notdeft-select-directory'."
  (interactive)
  (let ((dir (notdeft-select-directory)))
    (setq notdeft-directory (file-name-as-directory dir))
    (message "Data directory set to '%s'" notdeft-directory)))

;;;###autoload
(defun notdeft-open-file-by-basename (filename)
  "Open a NotDeft file named FILENAME.
FILENAME is a non-directory filename, with an extension
\(it is not necessarily unique)."
  (let ((fn (notdeft-file-by-basename filename)))
    (if (not fn)
	(message "No NotDeft note '%s'" filename)
      (notdeft-find-file fn))))

;;;###autoload
(defun notdeft-open-query (&optional query rank)
  "Open NotDeft with an Xapian search query.
If called interactively, read a search query interactively.
Non-interactively, the QUERY may be given as an argument. With a
non-nil RANK, have results ranked by relevance; interactively, a
prefix argument will set this option. Create a `notdeft-buffer'
if one does not yet exist, otherwise merely switch to the
existing one."
  (interactive "i\nP")
  (when notdeft-xapian-program
    (let ((query (or query (notdeft-xapian-read-query)))
	  (notdeft-xapian-order-by-time
	   (if rank nil notdeft-xapian-order-by-time)))
      (notdeft)
      (notdeft-xapian-query-set query))))

(defun notdeft-select-file/ido/nondirectory (files &optional prompt)
  "Present a choice of FILES with `ido-completing-read'.
Only present the non-directory component of each file.
There may be duplicates of the same non-directory name.
If given, use the specified PROMPT."
  (let ((choices
	 (mapcar
	  (lambda (file)
	    (propertize (file-name-nondirectory file) 'path file))
	  files)))
    (let ((file
	   (get-text-property
	    0 'path
	    (ido-completing-read (or prompt "File: ") choices nil t))))
      file)))

;;;###autoload
(defun notdeft-query-ido-find-file (&optional query)
  "Open one of the files matching Xapian search QUERY.
If called interactively, read a search query interactively,
accounting for `notdeft-xapian-query-history'. If there is more
than one match, present a choice list of non-directory filenames
with `ido-completing-read', ordering the choices by relevance."
  (interactive (list (notdeft-xapian-read-query)))
  (when notdeft-xapian-program
    (let* ((notdeft-xapian-order-by-time nil)
	   (files (notdeft-xapian-search notdeft-directories query)))
      (cond
       ((not files)
	(message "No matching notes found"))
       ((null (cdr files))
	(notdeft-find-file (car files)))
       (t
	(let ((file (notdeft-select-file/ido/nondirectory files)))
	  (when file
	    (notdeft-find-file file))))))))
    
;;;###autoload
(defun notdeft-lucky-find-file (&optional query)
  "Open the highest-ranked note matching a search QUERY.
If called interactively, read a search query interactively,
accounting for `notdeft-xapian-query-history'.
Open the file directly, without switching to any `notdeft-buffer'."
  (interactive (list (notdeft-xapian-read-query)))
  (when notdeft-xapian-program
    (let* ((notdeft-xapian-order-by-time nil)
	   (notdeft-xapian-max-results 1)
	   (files (notdeft-xapian-search notdeft-directories query)))
      (if (not files)
	  (message "No matching notes found")
	(notdeft-find-file (car files))))))

;;;###autoload
(defun notdeft-list-files-by-query (query)
  "Return a list of files matching Xapian QUERY."
  (when notdeft-xapian-program
    (notdeft-xapian-search notdeft-directories query)))

(provide 'notdeft)

(run-hooks 'notdeft-load-hook)

;;; notdeft.el ends here
