;;; bookmark.el --- Settings for `bookmark' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'bookmark)
(require 'al-places)

(setq
 bookmark-save-flag 1
 bookmark-default-file (al/emacs-data-dir-file "bookmarks"))

(defvar al/lazy-vertical-moving-map)

(al/bind-keys
  :map bookmark-bmenu-mode-map
  :parent al/lazy-vertical-moving-map
  ("→"   'bookmark-bmenu-relocate)
  ("d"   'bookmark-bmenu-other-window)
  ("C-d" 'bookmark-bmenu-switch-other-window)
  ("R"   'bookmark-bmenu-rename)
  ("z"   'bookmark-bmenu-unmark)
  ("D"   'bookmark-bmenu-delete)
  ("M-d" 'bookmark-bmenu-edit-annotation))

;;; bookmark.el ends here
