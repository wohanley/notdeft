;;; notdeft-mode-hydra.el --- Hydra for `notdeft-mode'
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; One possible definition of a hydra for `notdeft-mode'. Intended to
;; make it more convenient to execute multiple note file operations
;; consecutively, using short (mostly single letter) key combinations,
;; which are mostly the same as for `notdeft-global-hydra'. The
;; difference here is that the commands are mostly chainable.
;;
;; To set the hydra up for use, you may:
;;  (autoload 'notdeft-mode-hydra/body "notdeft-mode-hydra")
;;  (eval-after-load "notdeft"
;;   '(define-key notdeft-mode-map (kbd "C-c h") 'notdeft-mode-hydra/body))
;;
;; This feature will define nothing unless the `hydra' feature is
;; available.

;;; Code:

(require 'hydra nil t)
(require 'notdeft-autoloads)

(declare-function notdeft-select-file "notdeft")
(declare-function notdeft-gc "notdeft")
(declare-function notdeft-query-edit "notdeft")
(declare-function notdeft-query-clear "notdeft")
(declare-function notdeft-reindex "notdeft")
(declare-function notdeft-filter "notdeft")
(declare-function notdeft-filter-clear "notdeft")
(declare-function notdeft-grep-for-filter "notdeft")

(when (fboundp 'defhydra)
  (defhydra notdeft-mode-hydra ()
    "notdeft-mode"
    ;; file management
    ("RET" notdeft-select-file "open" :exit t)
    ("n" notdeft-new-file "create" :exit t)
    ("N" notdeft-new-file-named "create named" :exit t)
    ("d" notdeft-delete-file "delete")
    ("r" notdeft-rename-file "rename")
    ("m" notdeft-move-file "move")
    ("s" notdeft-move-into-subdir "move into subdir")
    ("e" notdeft-change-file-extension "change ext")
    ("a" notdeft-archive-file "archive")
    ("i" notdeft-show-file-directory "show dir")
    ("t" notdeft-open-in-deft "Deft" :exit t)
    ;; NotDeft state
    ("j" notdeft-chdir "chdir")
    ("C-g" notdeft-refresh "refresh")
    ("G" notdeft-gc "GC")
    ("R" notdeft-reindex "re-index")
    ;; filtering
    ("l" notdeft-filter "filter" :exit t)
    ("c" notdeft-filter-clear "clear filter")
    ("g" notdeft-grep-for-filter "grep for filter" :exit t)
    ;; querying
    ("o" notdeft-query-edit "query")
    ("O" notdeft-query-clear "clear query")
    ;; movement
    ("<up>" previous-line)
    ("<down>" next-line)
    ;; other
    ("b" notdeft-switch-to-note-buffer "switch to note" :exit t)
    ("x" quit-window "quit" :exit t)
    ("q" nil "cancel" :exit t)))

(provide 'notdeft-mode-hydra)

;;; notdeft-mode-hydra.el ends here
