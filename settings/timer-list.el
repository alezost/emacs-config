;;; timer-list.el --- Settings for `timer-list' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key))

(require 'timer-list)

(al/bind-keys
  :map timer-list-mode-map
  ("k"   . timer-list-cancel)
  ("C-k" . timer-list-cancel))

;;; timer-list.el ends here
