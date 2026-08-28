;;; image-mode.el --- Settings for `image-mode' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'image-mode)

(al/bind-keys
  :map image-mode-map
  ("C-⇤" 'image-bol)
  ("C-⇥" 'image-eol)
  ("h"   'image-previous-file)
  ("H"   'image-previous-frame)
  ("N"   'image-next-frame))

;;; image-mode.el ends here
