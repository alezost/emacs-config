;;; view.el --- Settings for `view' package  -*- lexical-binding: t -*-

(require 'view)
(require 'al-key)

(defvar al/lazy-moving-map)

(al/bind-keys
  :map view-mode-map
  :parent al/lazy-moving-map
  ("v" 'View-exit))

;;; view.el ends here
