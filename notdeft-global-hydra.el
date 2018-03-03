;;; notdeft-global-hydra.el --- Hydra for NotDeft
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; A hydra for some of NotDeft's globally usable commands; intended to
;; be “helpful” without much support for command chaining. Defines the
;; command `notdeft-global-hydra/body', and adds the hydra bindings to
;; an internal keymap. The `notdeft-global-hydra/body' command can be
;; bound as desired to access the commands with short key sequences
;; and textual hints in the Hydra command group context. It makes
;; available roughly the same bindings as in `notdeft-global-map', but
;; in a different way.
;;
;; To bind the hydra, one can for example:
;;   (autoload 'notdeft-global-hydra/body "notdeft-global-hydra" nil t)
;;   (global-set-key [f6] 'notdeft-global-hydra/body)
;;
;; This feature will define nothing unless the `hydra' feature is
;; available. If byte-compiled, `hydra' should be available during
;; compilation.

;;; Code:

(require 'hydra nil t)
(require 'notdeft-autoloads)

(when (fboundp 'defhydra) ;; if `require' has succeeded
  (defhydra notdeft-global-hydra (:exit t)
    "NotDeft"
    ;; file management
    ("n" notdeft-new-file "create")
    ("N" notdeft-new-file-named "create named")
    ("f" notdeft-find-file "open")
    ("w" notdeft-save-buffer "save" :exit nil)
    ("d" notdeft-delete-file "delete")
    ("r" notdeft-rename-file "rename")
    ("m" notdeft-move-file "move")
    ("s" notdeft-move-into-subdir "move into subdir")
    ("e" notdeft-change-file-extension "change ext")
    ("a" notdeft-archive-file "archive")
    ("i" notdeft-show-file-directory "show dir" :exit nil)
    ("t" notdeft-open-in-deft "Deft")
    ;; NotDeft state
    ("j" notdeft-chdir "chdir" :exit nil)
    ("g" notdeft-refresh "refresh" :exit nil)
    ;; search
    ("o" notdeft-open-query "search")
    ("l" notdeft-lucky-find-file "lucky search")
    ;; other
    ("v" notdeft "NotDeft")
    ("b" notdeft-switch-to-note-buffer "switch to note")
    ("B" notdeft-switch-to-buffer "switch to buffer")
    ("q" nil "cancel")))

(provide 'notdeft-global-hydra)

;;; notdeft-global-hydra.el ends here
