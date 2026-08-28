;;; flyspell.el --- Settings for `flyspell' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'flyspell)

(al/bind-keys
  :map flyspell-mode-map
  ("C-M-g n" 'flyspell-goto-next-error))

;;; flyspell.el ends here
