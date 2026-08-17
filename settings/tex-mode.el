;;; tex-mode.el --- Settings for `tex-mode' package  -*- lexical-binding: t -*-

(require 'tex-mode)
(require 'al-key)

(defconst al/tex-keys
  '("C-j")
  "Alist of auxiliary keys for `tex-mode-map'.")
(al/bind-keys-from-vars 'tex-mode-map 'al/tex-keys)

;;; tex-mode.el ends here
