;;; picture.el --- Settings for `picture' package  -*- lexical-binding: t -*-

(require 'picture)
(require 'al-key)

(defconst al/picture-keys
  '(("M-O" . picture-movement-left)
    ("M-U" . picture-movement-right)
    ("M->" . picture-movement-up)
    ("M-E" . picture-movement-down)
    ("M-<" . picture-movement-nw)
    ("M-P" . picture-movement-ne)
    ("M-Q" . picture-movement-sw)
    ("M-K" . picture-movement-se))
  "Alist of auxiliary keys for `picture-mode-map'.")
(al/bind-keys-from-vars 'picture-mode-map 'al/picture-keys)

;;; picture.el ends here
