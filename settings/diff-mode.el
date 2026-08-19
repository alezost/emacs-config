;;; diff-mode.el --- Settings for `diff-mode' package  -*- lexical-binding: t -*-

(require 'diff-mode)
(require 'al-key)

(defconst al/diff-shared-keys
  '(("." . diff-hunk-prev)
    (">" . diff-file-prev)
    ("e" . diff-hunk-next)
    ("E" . diff-file-next))
  "Alist of auxiliary keys for `diff-mode-shared-map'.")
(al/bind-keys-from-vars 'diff-mode-shared-map 'al/diff-shared-keys t)

(defconst al/diff-keys
  '(("H-u" . diff-undo)
    ("M-." . diff-hunk-prev)
    ("M->" . diff-file-prev)
    ("M-e" . diff-hunk-next)
    ("M-E" . diff-file-next))
  "Alist of auxiliary keys for `diff-mode-map'.")
(al/bind-keys-from-vars 'diff-mode-map 'al/diff-keys)

;;; diff-mode.el ends here
