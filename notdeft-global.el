;;; notdeft-global.el --- Global NotDeft keymap
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; A keymap of NotDeft commands usable from outside `notdeft-mode'. It is
;; bound both as a variable and a function, to the name
;; `notdeft-global-name'.

;;; Code:

(defvar notdeft-global-map
  (let ((map (make-sparse-keymap)))
    ;; File creation, opening, saving
    (define-key map (kbd "C-n") 'notdeft-new-file)
    (define-key map (kbd "C-m") 'notdeft-new-file-named)
    (define-key map (kbd "C-f") 'notdeft-find-file)
    (define-key map (kbd "C-w") 'notdeft-save-buffer)
    ;; File management
    (define-key map (kbd "C-d") 'notdeft-delete-file)
    (define-key map (kbd "C-r") 'notdeft-rename-file)
    (define-key map (kbd "S") 'notdeft-move-into-subdir)
    (define-key map (kbd "m") 'notdeft-move-file)
    (define-key map [(control x) (e)] 'notdeft-change-file-extension)
    (define-key map (kbd "C-a") 'notdeft-archive-file)
    ;; Xapian search
    (define-key map (kbd "f") 'notdeft-lucky-find-file)
    (define-key map (kbd "o") 'notdeft-open-query)
    ;; Miscellaneous
    (define-key map (kbd "e") 'notdeft)
    (define-key map (kbd "C-j") 'notdeft-chdir)
    (define-key map (kbd "C-g") 'notdeft-refresh)
    map)
  "Global keymap for NotDeft.

\\{notdeft-global-map}")
(fset 'notdeft-global-map notdeft-global-map)

(provide 'notdeft-global)

;;; notdeft-global.el ends here
