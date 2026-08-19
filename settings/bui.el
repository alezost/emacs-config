;;; bui.el --- Settings for `bui' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'bui-core)
(require 'al-key)

(defconst al/bui-keys
  '(("," . bui-history-back)
    ("p" . bui-history-forward))
  "Alist of auxiliary keys for `bui-map'.")
(al/bind-keys-from-vars 'bui-map 'al/bui-keys)

(al/eval-after-load bui-list
  (defconst al/bui-list-keys
    '(("u" . bui-list-describe)
      ("z" . bui-list-unmark)
      ("Z" . bui-list-unmark-all))
    "Alist of auxiliary keys for `bui-list-mode-map'.")
  (al/bind-keys-from-vars 'bui-list-mode-map 'al/bui-list-keys))

;;; bui.el ends here
