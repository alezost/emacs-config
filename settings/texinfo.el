;;; texinfo.el --- Settings for `texinfo' package  -*- lexical-binding: t -*-

(require 'texinfo)
(require 'al-key)
(require 'al-texinfo)

(defconst al/texinfo-keys
  '(("C-c c" . texinfo-insert-@code)
    ("C-c f" . texinfo-insert-@file)
    ("C-c i" . texinfo-insert-@item)
    ("C-c v" . texinfo-insert-@var)
    ("C-c M" . al/texinfo-insert-@menu)
    ("C-c E" . al/texinfo-insert-@example)
    ("C-c I" . al/texinfo-insert-@itemize)
    ("C-c T" . al/texinfo-insert-@table)
    ("C-c D" . al/texinfo-insert-@deffn))
  "Alist of auxiliary keys for `texinfo-mode'.")
(al/bind-keys-from-vars 'texinfo-mode-map 'al/texinfo-keys)

;;; texinfo.el ends here
