;;; shr.el --- Settings for `shr' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'shr)
(require 'al-shr)

(al/bind-keys
  :map shr-map
  ("→" 'al/shr-browse-this-url)
  ("c" 'shr-copy-url))

;;; shr.el ends here
