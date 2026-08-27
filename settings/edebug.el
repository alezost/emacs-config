;;; edebug.el --- Settings for `edebug' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key))

(require 'edebug)
(require 'al-general)

(al/bind-keys
  :map edebug-mode-map
  ("v"   'edebug-eval-expression)
  ("C-v" 'edebug-eval-last-sexp))

;;; edebug.el ends here
