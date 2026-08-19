;;; tabulated-list.el --- Settings for `tabulated-list' package  -*- lexical-binding: t -*-

(require 'tabulated-list)
(require 'al-key)

(defconst al/tabulated-list-keys
  '(("s" . tabulated-list-sort))
  "Alist of auxiliary keys for `tabulated-list-mode-map'.")
(al/bind-keys-from-vars 'tabulated-list-mode-map
  '(al/lazy-moving-keys al/tabulated-list-keys)
  t)

(add-hook 'tabulated-list-mode-hook #'hl-line-mode)

;;; tabulated-list.el ends here
