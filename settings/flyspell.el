;;; flyspell.el --- Settings for `flyspell' package  -*- lexical-binding: t -*-

(require 'flyspell)
(require 'al-key)

(defconst al/flyspell-keys
  '(("C-M-g n" . flyspell-goto-next-error))
  "Alist of auxiliary keys for `flyspell-mode-map'.")
(al/bind-keys-from-vars 'flyspell-mode-map 'al/flyspell-keys)

;;; flyspell.el ends here
