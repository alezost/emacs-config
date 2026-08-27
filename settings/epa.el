;;; epa.el --- Settings for `epa' package  -*- lexical-binding: t -*-

(require 'epa)
(require 'al-key)

(al/bind-keys
  :map epa-key-list-mode-map
  :parent button-buffer-map
  ("→" 'epa-show-key)
  ("z" 'epa-unmark-key))

;;; epa.el ends here
