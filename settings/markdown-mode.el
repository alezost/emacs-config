;;; markdown-mode.el --- Settings for `markdown-mode' package  -*- lexical-binding: t -*-

(require 'markdown-mode)
(require 'al-key)

(defconst al/markdown-keys
  '(("M->" . markdown-previous-link)
    ("M-E" . markdown-next-link)))

(al/bind-keys-from-vars 'markdown-mode-map 'al/markdown-keys)

;;; markdown-mode.el ends here
