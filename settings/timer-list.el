;;; timer-list.el --- Settings for `timer-list' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'timer-list)
(require 'al-general)

(al/bind-keys
  :map timer-list-mode-map
  :parent special-mode-map
  :clean t
  ("k"   'timer-list-cancel)
  ("C-k" 'timer-list-cancel))

;;; timer-list.el ends here
