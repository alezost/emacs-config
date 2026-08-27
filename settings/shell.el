;;; shell.el --- Settings for `shell' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'shell)
(require 'sh-script)
(require 'al-places)
(require 'al-general)
(require 'al-key)
(require 'al-shell)

(al/bind-keys
  :map shell-mode-map
  "M-?"
  ("M-S-←" 'shell-backward-command)
  ("M-S-→" 'shell-forward-command))

(setq
 ;; `sh-mode-syntax-table' has proper syntax for comments.
 shell-mode-syntax-table sh-mode-syntax-table)

(al/call-at-hook shell-mode-hook
  abbrev-mode
  al/no-truncate-lines
  al/shell-set-local-variables)

;;; shell.el ends here
