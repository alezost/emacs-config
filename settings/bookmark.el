;;; bookmark.el --- Settings for `bookmark' package  -*- lexical-binding: t -*-

(require 'bookmark)
(require 'al-places)
(require 'al-key)

(setq
 bookmark-save-flag 1
 bookmark-default-file (al/emacs-data-dir-file "bookmarks"))

(defconst al/bookmark-keys
  '(("u"   . bookmark-bmenu-relocate)
    ("d"   . bookmark-bmenu-other-window)
    ("C-d" . bookmark-bmenu-switch-other-window)
    ("R"   . bookmark-bmenu-rename)
    ("z"   . bookmark-bmenu-unmark)
    ("D"   . bookmark-bmenu-delete)
    ("M-d" . bookmark-bmenu-edit-annotation)))

(al/bind-keys-from-vars 'bookmark-bmenu-mode-map
  '(al/lazy-moving-keys al/bookmark-keys))

;;; bookmark.el ends here
