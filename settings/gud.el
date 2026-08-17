;;; gud.el --- Settings for `gud' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'gud)
(require 'al-key)

;; GUD binds its keys inside `gdb' and `gud-gdb' commands.
(al/call-at-hook (gdb-mode-hook
                  gud-gdb-mode-hook)
  (al/bind-keys-from-vars 'gud-mode-map 'al/comint-keys))

;;; gud.el ends here
