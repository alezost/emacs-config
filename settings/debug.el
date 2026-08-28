;;; debug.el --- Settings for `debug' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'debug)

(al/bind-keys
  :map debugger-mode-map
  ("v" 'debugger-eval-expression)
  ("l" 'debugger-toggle-locals)
  ("f" 'debugger-list-functions))

;;; debug.el ends here
