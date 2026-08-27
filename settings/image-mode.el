;;; image-mode.el --- Settings for `image-mode' package  -*- lexical-binding: t -*-

(require 'image-mode)
(require 'al-key)

(al/bind-keys
  :map image-mode-map
  ("C-⇤" 'image-bol)
  ("C-⇥" 'image-eol)
  ("h"   'image-previous-file)
  ("H"   'image-previous-frame)
  ("N"   'image-next-frame))

;;; image-mode.el ends here
