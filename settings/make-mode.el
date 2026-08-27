;;; make-mode.el --- Settings for `make-mode' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key))

(require 'make-mode)
(require 'al-general)

(al/bind-keys
  :map makefile-mode-map
  ("M-S-↑" 'makefile-previous-dependency)
  ("M-S-↓" 'makefile-next-dependency))

;;; make-mode.el ends here
