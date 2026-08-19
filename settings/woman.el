;;; woman.el --- Settings for `woman' package  -*- lexical-binding: t -*-

(require 'woman)
(require 'al-key)

(defconst al/woman-keys
  '(("M-h" . WoMan-previous-manpage))
  "Alist of auxiliary keys for `woman-mode'.")
(al/bind-keys-from-vars 'woman-mode-map 'al/woman-keys)

(setq
 woman-fill-column (default-value 'fill-column)
 woman-default-indent 4)

;;; woman.el ends here
