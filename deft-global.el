;;; deft-global.el --- Global Deft keymap
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "deft.el" for licensing information.

;;; Commentary:
;; A keymap of Deft commands usable from outside `deft-mode'. It is
;; bound both as a variable and a function, to the name
;; `deft-global-name'.

(defvar deft-global-map
  (let ((map (make-sparse-keymap)))
    ;; File creation, opening, saving
    (define-key map (kbd "C-n") 'deft-new-file)
    (define-key map (kbd "C-m") 'deft-new-file-named)
    (define-key map (kbd "C-f") 'deft-find-file)
    (define-key map (kbd "C-w") 'deft-save-buffer)
    ;; File management
    (define-key map (kbd "C-d") 'deft-delete-file)
    (define-key map (kbd "C-r") 'deft-rename-file)
    (define-key map (kbd "S") 'deft-move-into-subdir)
    (define-key map (kbd "m") 'deft-move-file)
    (define-key map [(control x) (e)] 'deft-change-file-extension)
    (define-key map (kbd "C-a") 'deft-archive-file)
    ;; Xapian search
    (define-key map (kbd "f") 'deft-lucky-find-file)
    (define-key map (kbd "o") 'deft-open-query)
    ;; Miscellaneous
    (define-key map (kbd "e") 'deft)
    (define-key map (kbd "C-j") 'deft-chdir)
    (define-key map (kbd "C-g") 'deft-refresh)
    map)
  "Global keymap for Deft.

\\{deft-global-map}")
(fset 'deft-global-map deft-global-map)

(provide 'deft-global)

;;; deft-global.el ends here
