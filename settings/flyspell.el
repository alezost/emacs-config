;;; flyspell.el --- Settings for `flyspell' package  -*- lexical-binding: t -*-

(require 'flyspell)
(require 'al-key)

(al/bind-keys
  :map flyspell-mode-map
  ("C-M-g n" 'flyspell-goto-next-error))

;;; flyspell.el ends here
