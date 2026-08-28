;;; view.el --- Settings for `view' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'view)

(defvar al/lazy-moving-map)

(al/bind-keys
  :map view-mode-map
  :parent al/lazy-moving-map
  ("v" 'View-exit))

;;; view.el ends here
