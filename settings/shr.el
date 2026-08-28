;;; shr.el --- Settings for `shr' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'shr)

(al/bind-keys
  :map shr-map
  ("→" 'shr-browse-url)
  ("c" 'shr-copy-url))

;;; shr.el ends here
