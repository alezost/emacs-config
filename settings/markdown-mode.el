;;; markdown-mode.el --- Settings for `markdown-mode' package  -*- lexical-binding: t -*-

(require 'markdown-mode)
(require 'al-key)

(al/bind-keys
  :map markdown-mode-map
  ("M-S-↑" 'markdown-previous-link)
  ("M-S-↓" 'markdown-next-link))

;;; markdown-mode.el ends here
