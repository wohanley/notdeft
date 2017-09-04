;;; deft-autoloads.el --- autoloads for Deft

;;; Commentary:
;; Run "make autoloads" to regenerate this file.

;;; Code:

;;;### (autoloads nil "deft" "deft.el" (22957 34243 0 0))
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

(autoload 'deft-save-buffer "deft" "\
Save the current buffer.
The prefix argument PFX is passed to `save-buffer'.
Set up a hook for refreshing Deft state on save.

\(fn PFX)" t nil)

(autoload 'deft-rename-current-file "deft" "\
Rename current buffer file in a Deft-aware manner.
Query for a new name, using any parsed title to derive
the default name; otherwise default to the old basename.

\(fn)" t nil)

(autoload 'deft-refresh "deft" "\
Refresh or reset Deft state.
Refresh Deft state so that filesystem changes get noticed.
With a PREFIX argument, reset state, so that caches and
queries and such are also cleared.
Invoke this command manually if Deft files change outside of
`deft-mode', as such changes are not detected automatically.

\(fn PREFIX)" t nil)

(autoload 'deft "deft" "\
Switch to `deft-buffer', creating it if not yet created.
With a prefix argument PFX, always query for an initial
`deft-directory' choice for a newly created Deft buffer,
and otherwise query only as necessary.

\(fn &optional PFX)" t nil)

(autoload 'deft-read-extension "deft" "\
Read a Deft filename extension, interactively.
The default choice is `deft-extension', but any of the
`deft-secondary-extensions' are also available as choices.

\(fn)" nil nil)

(autoload 'deft-select-directory "deft" "\
Select a Deft directory, possibly interactively.
Select from the configured list of directories (i.e., `deft-path');
any DIRS argument overrides the configured list of choices.
Non-existing directories are not available for selecting.
If `default-directory' is a Deft one, use that as the default choice.
Return the selected directory, or error out.

\(fn &optional DIRS)" nil nil)

(autoload 'deft-chdir "deft" "\
Change `deft-directory' according to interactive selection.

\(fn)" t nil)

(autoload 'deft-open-file-by-basename "deft" "\
Open a Deft file named FILENAME.
FILENAME is a non-directory filename, with an extension
\(it is not necessarily unique).

\(fn FILENAME)" nil nil)

(autoload 'deft-open-query "deft" "\
Open Deft with an interactively read Xapian search query.
Create a `deft-buffer' if one does not yet exist,
otherwise merely switch to the existing Deft buffer.

\(fn)" t nil)

(autoload 'deft-lucky-find-file "deft" "\
Open the highest-ranked note matching a search query.
Read the query interactively, accounting for `deft-xapian-query-history'.
Open the file directly, without switching to any `deft-buffer'.

\(fn)" t nil)

;;;***

(provide 'deft-autoloads)

;;; deft-autoloads.el ends here
