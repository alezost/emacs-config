;;; picture.el --- Settings for `picture' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'picture)

(al/bind-keys
  :map picture-mode-map
  ("M-S-←" 'picture-movement-left)
  ("M-S-→" 'picture-movement-right)
  ("M-S-↑" 'picture-movement-up)
  ("M-S-↓" 'picture-movement-down)
  ("M-S-↶" 'picture-movement-nw)
  ("M-S-↷" 'picture-movement-ne)
  ("M-S-↤" 'picture-movement-sw)
  ("M-S-↦" 'picture-movement-se))

;;; picture.el ends here
