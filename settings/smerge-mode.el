;;; smerge-mode.el --- Settings for `smerge-mode' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'smerge-mode)

(al/bind-keys
  :map smerge-mode-map
  ("C-c C-↑" 'smerge-keep-upper)
  ("C-c C-↓" 'smerge-keep-lower))

;;; smerge-mode.el ends here
