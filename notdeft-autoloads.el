;;; notdeft-autoloads.el --- autoloads for NotDeft

;;; Commentary:
;; Run "make autoloads" to regenerate this file.

;;; Code:

;;;### (autoloads nil "notdeft" "notdeft.el" (23796 6841 389000 0))
;;; Generated autoloads from notdeft.el

(let ((loads (get 'notdeft 'custom-loads))) (if (member '"notdeft" loads) nil (put 'notdeft 'custom-loads (cons '"notdeft" loads))))

(autoload 'notdeft-title-from-file-content "notdeft" "\
Extract a title from FILE content.
Return nil on failure.

\(fn FILE)" nil nil)

(autoload 'notdeft-chomp "notdeft" "\
Trim leading and trailing whitespace from STR.

\(fn STR)" nil nil)

(autoload 'notdeft-chomp-nullify "notdeft" "\
Return string STR if non-empty, otherwise return nil.
Optionally, use function TRIM to trim any result string.

\(fn STR &optional TRIM)" nil nil)

(autoload 'notdeft-file-by-basename "notdeft" "\
Resolve a NotDeft note NAME to a full pathname.
NAME is a non-directory filename, with extension. Resolve it to
the path of a file under `notdeft-directories' or
`notdeft-sparse-directories', if such a note file does exist. If
multiple such files exist, return one of them. If none exist,
return nil.

\(fn NAME)" nil nil)

(autoload 'notdeft-make-basename-list "notdeft" "\
Return the names of all NotDeft notes.
Search all existing `notdeft-directories', and include all
existing `notdeft-sparse-directories' files. The result list is
sorted by the `string-lessp' relation, and it may contain
duplicates.

\(fn)" nil nil)

(autoload 'notdeft-note-mode "notdeft" "\
Manage NotDeft state for a note buffer.
A minor mode that should be enabled for NotDeft notes. Does
nothing but manage calls to `notdeft-register-buffer' and
`notdeft-deregister-buffer', which allow NotDeft to keep track of
changes to its note buffers.

\(fn &optional ARG)" t nil)

(autoload 'notdeft-register-file "notdeft" "\
Enable NotDeft note mode for any buffer of FILE.

\(fn FILE)" nil nil)

(autoload 'notdeft-save-buffer "notdeft" "\
Save the current buffer as a NotDeft note.
Enable NotDeft note minor mode before saving.
The PREFIX argument is passed to `save-buffer'.

\(fn PREFIX)" t nil)

(autoload 'notdeft-switch-to-buffer "notdeft" "\
Switch to an existing NotDeft buffer.
Where multiple buffers exist, query for the desired buffer
interactively.

\(fn)" t nil)

(autoload 'notdeft-switch-to-note-buffer "notdeft" "\
Switch to an existing NotDeft note buffer.
The list of choices is determined by the function
`notdeft-note-buffer-list'.

\(fn)" t nil)

(autoload 'notdeft-find-file "notdeft" "\
Edit NotDeft note FILE.
Enable NotDeft note mode for the buffer for editing the file.
Called interactively, query for the FILE using the minibuffer.

\(fn FILE)" t nil)

(autoload 'notdeft-create-file "notdeft" "\
Create a new NotDeft note file.
Create it into the directory DIR with basename NOTENAME and
filename extension EXT, and write any DATA into the file. If any
of those values are nil, then use a default value. If DIR or EXT
is the symbol `ask', then query the user for a directory or
extension. If DIR is a non-empty list, then offer the user that
choice list of directories. If NOTENAME is of the form (format
FMT), then use `notdeft-generate-filename' to generate a filename
with the format string FMT. If NOTENAME is of the form (title
STR), then use `notdeft-title-to-notename' to generate a notename
from STR.

\(fn &optional DIR NOTENAME EXT DATA)" nil nil)

(autoload 'notdeft-switch-to-file-named "notdeft" "\
Switch to a NotDeft note with the specified TITLE.
Derive a note name from the title with
`notdeft-title-to-notename', or fail that cannot be done. If no
note of the derived named exists, create one. Initialize any
created file with DATA, or TITLE if not given. Return the full
file name of the file.

\(fn TITLE &optional DATA)" nil nil)

(autoload 'notdeft-new-file-named "notdeft" "\
Create a new file, prompting for a title.
The prefix argument PFX is as for `notdeft-new-file'.
Query for a TITLE when invoked as a command.
Initialize the file with DATA, or TITLE if not given.
Return the filename of the created file.

\(fn PFX TITLE &optional DATA)" t nil)

(autoload 'notdeft-new-file "notdeft" "\
Create a new file quickly.
Create it with an automatically generated name, one based
on the `notdeft-filter-string' filter string if it is non-nil.
With a prefix argument PFX, offer a choice of NotDeft
directories, when there is more than one of them.
With two prefix arguments, also offer a choice of filename
extensions when `notdeft-secondary-extensions' is non-empty.
Return the filename of the created file.

\(fn PFX)" t nil)

(autoload 'notdeft-delete-file "notdeft" "\
Delete the selected or current NotDeft note file.
Prompt before proceeding. With a PREFIX argument, also kill the
deleted file's buffer, if any.

\(fn PREFIX)" t nil)

(autoload 'notdeft-move-into-subdir "notdeft" "\
Move the file at point into a subdirectory of the same name.
To nest more than one level (which is allowed but perhaps atypical),
invoke with a PREFIX argument to force the issue.

\(fn PREFIX)" t nil)

(autoload 'notdeft-change-file-extension "notdeft" "\
Change the filename extension of a NotDeft note.
Operate on the selected or current NotDeft note file.

\(fn)" t nil)

(autoload 'notdeft-rename-file "notdeft" "\
Rename the selected or current NotDeft note file.
Defaults to a content-derived file name (rather than the old one)
if called with a prefix argument PFX.

\(fn PFX)" t nil)

(autoload 'notdeft-move-file "notdeft" "\
Move the selected file under selected NotDeft root.
Query the user for a target from among `notdeft-directories'.
Offer to create the chosen NotDeft root directory if it does not
already exist. If the file resides in a subdirectory, move the
entire subdirectory, but require confirmation as a non-nil PFX
argument, or by asking. Moving an external (non-Deft) file under
a NotDeft root is also allowed.

\(fn &optional PFX)" t nil)

(autoload 'notdeft-archive-file "notdeft" "\
Archive the selected NotDeft note file.
Archive it under `notdeft-archive-directory', under its NotDeft
root directory. If it resides in a subdirectory, archive the
entire directory, but require confirmation as a non-nil PFX
argument, or by asking the user when called interactively.

\(fn &optional PFX)" t nil)

(autoload 'notdeft-open-in-deft "notdeft" "\
Open the selected note's Deft directory in Deft.
Do that only when the command `deft' is available. This
implementation makes assumptions about Deft.

\(fn)" t nil)

(autoload 'notdeft-show-file-directory "notdeft" "\
Show NotDeft directory of the selected note.

\(fn)" t nil)

(autoload 'notdeft-refresh "notdeft" "\
Refresh or reset NotDeft state.
Refresh NotDeft state so that outside filesystem changes get
noticed. With a non-nil prefix argument RESET, also reset state
to clear caches and queries and such. With two prefix arguments,
clear queries and filters for all NotDeft mode buffers. Invoke
this command manually if NotDeft files change outside of NotDeft
mode and NotDeft note minor mode (as toggled by the command
`notdeft-mode' and the command `notdeft-note-mode'), as such
changes are not detected automatically. Also invoke this if you
change `notdeft-directories' or `notdeft-sparse-directories'.

\(fn &optional RESET)" t nil)

(autoload 'notdeft "notdeft" "\
Switch to a `notdeft-buffer', creating one if not yet created.
With a non-nil argument RESET, switch to any existing NotDeft
buffer with fresh state. With a non-nil argument NEW, always
create a new buffer, even when a `notdeft-buffer' already exists.
When called interactively, one prefix argument means NEW, whereas
two prefix arguments means RESET.

\(fn &optional RESET NEW)" t nil)

(autoload 'notdeft-read-extension "notdeft" "\
Read a NotDeft filename extension, interactively.
The default choice is `notdeft-extension', but any of the
`notdeft-secondary-extensions' are also available as choices.
With a PREFER argument, use that extension as the first choice.

\(fn &optional PREFER)" nil nil)

(autoload 'notdeft-select-directory "notdeft" "\
Select a NotDeft directory, possibly interactively.
If DIRS is non-nil, select from among those directories;
otherwise select from `notdeft-directories'. Use the specified
PROMPT in querying, if given. Return the selected directory, or
error out. If CONFIRM is non-nil, query even if there is only a
single choice. Present any `notdeft-directory' as the first
choice, except with a true PRESERVE argument, which preserves
DIRS order.

\(fn &optional PROMPT CONFIRM PRESERVE)" nil nil)

(autoload 'notdeft-chdir "notdeft" "\
Change `notdeft-directory' according to interactive selection.
Query for a directory with `notdeft-select-directory'.

\(fn)" t nil)

(autoload 'notdeft-open-file-by-basename "notdeft" "\
Open a NotDeft file named FILENAME.
FILENAME is a non-directory filename, with an extension
\(it is not necessarily unique).

\(fn FILENAME)" nil nil)

(autoload 'notdeft-open-query "notdeft" "\
Open NotDeft with an Xapian search QUERY.
When called interactively, read the QUERY interactively. With
non-nil RANK, have results ranked by relevance; when called
interactively, the command prefix \\[universal-argument] 1 will
set this option. Open the query in a new buffer as specified by
the `notdeft-open-query-in-new-buffer' configuration option; a
non-nil NEGATE argument reverses that setting, as does the prefix
\\[universal-argument] when called interactively.

\(fn &optional QUERY RANK NEGATE)" t nil)

(autoload 'notdeft-query-ido-find-file "notdeft" "\
Open one of the files matching Xapian search QUERY.
If called interactively, read a search query interactively,
accounting for `notdeft-xapian-query-history'. If there is more
than one match, present a choice list of non-directory filenames
with `ido-completing-read'. Order the choices by relevance, or
BY-TIME if requested.

\(fn &optional QUERY BY-TIME)" t nil)

(autoload 'notdeft-lucky-find-file "notdeft" "\
Open the highest-ranked note matching a search QUERY.
If called interactively, read a search query interactively,
accounting for `notdeft-xapian-query-history'.
Open the file directly, without switching to any `notdeft-buffer'.

\(fn &optional QUERY)" t nil)

(autoload 'notdeft-list-files-by-query "notdeft" "\
Return a list of files matching Xapian QUERY.

\(fn QUERY)" nil nil)

;;;***

(provide 'notdeft-autoloads)

;;; notdeft-autoloads.el ends here
