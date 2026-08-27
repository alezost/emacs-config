;;; woman.el --- Settings for `woman' package  -*- lexical-binding: t -*-

(require 'woman)
(require 'al-key)

(al/bind-keys
  :map woman-mode-map
  ("M-h" 'WoMan-previous-manpage))

(setq
 woman-fill-column (default-value 'fill-column)
 woman-default-indent 4)

;;; woman.el ends here
