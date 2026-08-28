;;; epa.el --- Settings for `epa' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'epa)

(al/bind-keys
  :map epa-key-list-mode-map
  :parent button-buffer-map
  ("→" 'epa-show-key)
  ("z" 'epa-unmark-key))

;;; epa.el ends here
