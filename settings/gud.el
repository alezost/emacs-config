;;; gud.el --- Settings for `gud' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'gud)
(require 'al-key)

;; GUD binds its keys inside `gdb' and `gud-gdb' commands.
(al/call-at-hook (gdb-mode-hook
                  gud-gdb-mode-hook)
  (al/bind-keys
    :map gud-mode-map
    :parent comint-mode-map))

;;; gud.el ends here
