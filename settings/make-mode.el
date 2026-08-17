;;; make-mode.el --- Settings for `make-mode' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key))

(require 'make-mode)

(al/bind-keys
  :map makefile-mode-map
  ("M->" . makefile-previous-dependency)
  ("M-E" . makefile-next-dependency))

;;; make-mode.el ends here
