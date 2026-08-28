;;; markdown-mode.el --- Settings for `markdown-mode' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'markdown-mode)

(al/bind-keys
  :map markdown-mode-map
  ("M-S-↑" 'markdown-previous-link)
  ("M-S-↓" 'markdown-next-link))

;;; markdown-mode.el ends here
