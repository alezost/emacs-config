;;; debug.el --- Settings for `debug' package  -*- lexical-binding: t -*-

(require 'debug)
(require 'al-key)

(al/bind-keys-from-vars 'debugger-mode-map 'al/button-keys t)

(al/bind-keys
  :map debugger-mode-map
  ("v" . debugger-eval-expression)
  ("l" . debugger-toggle-locals)
  ("f" . debugger-list-functions))

;;; debug.el ends here
