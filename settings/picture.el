;;; picture.el --- Settings for `picture' package  -*- lexical-binding: t -*-

(require 'picture)
(require 'al-key)

(al/bind-keys
  :map picture-mode-map
  ("M-S-←" 'picture-movement-left)
  ("M-S-→" 'picture-movement-right)
  ("M-S-↑" 'picture-movement-up)
  ("M-S-↓" 'picture-movement-down)
  ("M-S-↷" 'picture-movement-nw)
  ("M-S-↶" 'picture-movement-ne)
  ("M-Q" 'picture-movement-sw)
  ("M-K" 'picture-movement-se))

;;; picture.el ends here
