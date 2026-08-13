;;; image-mode.el --- Settings for `image-mode' package  -*- lexical-binding: t -*-

(require 'image-mode)
(require 'al-key)

(defconst al/image-keys
  '(("C-a" . image-bol)
    ("<ctrl-i>" . image-eol)
    ("h"   . image-previous-file)
    ("H"   . image-previous-frame)
    ("N"   . image-next-frame)))

(al/bind-keys-from-vars 'image-mode-map 'al/image-keys)

;;; image-mode.el ends here
