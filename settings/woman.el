;;; woman.el --- Settings for `woman' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'woman)

(al/bind-keys
  :map woman-mode-map
  ("M-h" 'WoMan-previous-manpage))

(setq
 woman-fill-column (default-value 'fill-column)
 woman-default-indent 4)

;;; woman.el ends here
