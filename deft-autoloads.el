;;; deft-autoloads.el --- autoloads for Deft

;;; Commentary:
;; Run "make autoloads" to regenerate this file.

;;; Code:

;;;### (autoloads nil "deft" "deft.el" (22954 31202 0 0))
;;; Generated autoloads from deft.el

(autoload 'deft-title-from-file-content "deft" "\
Extract a title from FILE content.
Return nil on failure.

\(fn FILE)" nil nil)

(autoload 'deft-file-by-basename "deft" "\
Resolve a Deft note NAME to a full pathname.
NAME is a non-directory filename, with extension.
Resolve it to the path of a file under a `deft-path'
directory, if such a note file does exist.
If multiple such files exist, return one of them.
If none exist, return nil.

\(fn NAME)" nil nil)

(autoload 'deft-make-basename-list "deft" "\
Return the names of all Deft notes.
Search all existing `deft-path' directories.
The result list is sorted by the `string-lessp' relation.
It may contain duplicates.

\(fn)" nil nil)

(autoload 'deft-chomp-nullify "deft" "\
Return string STR if non-empty, otherwise return nil.
Optionally, use function TRIM to trim any result string.

\(fn STR &optional TRIM)" nil nil)

(autoload 'deft-open-file "deft" "\
Open FILE in a new buffer and set its mode.
Set up a hook for refreshing Deft state on save.

\(fn FILE)" nil nil)

(autoload 'deft-rename-current-file "deft" "\
Rename current buffer file in a Deft-aware manner.
Query for a new name, using any parsed title to derive
the default name; otherwise default to the old basename.

\(fn)" t nil)

(autoload 'deft-select-directory "deft" "\
Select a Deft directory, possibly interactively.
Select from the configured list of directories (i.e., `deft-path'),
possibly user assisted.
Any DIRS argument overrides the list of choices.
\(Non-existing directories are not available for selecting.)
If `default-directory' is a Deft one, use that as the default choice.
Return the selected directory, or error out.

\(fn &optional DIRS)" nil nil)

(autoload 'deft-open-file-by-basename "deft" "\
Open a Deft file named FILENAME.
FILENAME is a non-directory filename, with an extension
\(it is not necessarily unique).

\(fn FILENAME)" nil nil)

(autoload 'deft "deft" "\
Create `deft-buffer' and initialize Deft.
Switch to the buffer.
Reset state even if the buffer already exists.
With a prefix argument PFX, always query for the initial
`deft-directory' choice, and otherwise query only as necessary.

\(fn &optional PFX)" t nil)

(autoload 'deft-switch-to-buffer "deft" "\
Switch to `deft-buffer'.
Create it if it does not exist.

\(fn)" t nil)

(autoload 'deft-open-query "deft" "\
Open Deft with an interactively read Xapian search QUERY.
Start Deft up if no `deft-buffer' yet exists,
otherwise merely switch to the existing buffer.

\(fn)" t nil)

(autoload 'deft-lucky-find-file "deft" "\
Open the highest-ranked note matching a search query.
Read the query interactively, accounting for `deft-xapian-query-history'.
Open the file directly, without switching to any `deft-buffer'.
Do not modify the `deft-buffer', or modify Deft state.

\(fn)" t nil)

;;;***

(provide 'deft-autoloads)

;;; deft-autoloads.el ends here
