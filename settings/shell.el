;;; shell.el --- Settings for `shell' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'shell)
(require 'sh-script)
(require 'al-places)
(require 'al-general)
(require 'al-key)
(require 'al-shell)

(defconst al/shell-keys
  '("TAB" "M-?"
    ("M-O" . shell-backward-command)
    ("M-U" . shell-forward-command))
  "Alist of auxiliary keys for `shell-mode-map'.")
(al/bind-keys-from-vars 'shell-mode-map 'al/shell-keys)

(setq
 ;; `sh-mode-syntax-table' has proper syntax for comments.
 shell-mode-syntax-table sh-mode-syntax-table)

(al/call-at-hook shell-mode-hook
  abbrev-mode
  al/no-truncate-lines
  al/shell-set-local-variables)

;;; shell.el ends here
