;;; diff-mode.el --- Settings for `diff-mode' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'diff-mode)

(al/bind-keys
  :map diff-mode-shared-map
  ("↑"   'diff-hunk-prev)
  ("S-↑" 'diff-file-prev)
  ("↓"   'diff-hunk-next)
  ("S-↓" 'diff-file-next))

(al/bind-keys
  :map diff-mode-map
  ("H-u"   'diff-undo)
  ("M-↑"   'diff-hunk-prev)
  ("M-S-↑" 'diff-file-prev)
  ("M-↓"   'diff-hunk-next)
  ("M-S-↓" 'diff-file-next))

;;; diff-mode.el ends here
