;;; view.el --- Settings for `view' package  -*- lexical-binding: t -*-

(require 'view)
(require 'al-key)

(defconst al/view-keys
  '(("v" . View-exit))
  "Alist of auxiliary keys for `view-mode-map'.")
(al/bind-keys-from-vars 'view-mode-map
  '(al/lazy-moving-keys al/view-keys)
  t)

;;; view.el ends here
