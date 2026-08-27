;;; bui.el --- Settings for `bui' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'bui-core)
(require 'al-key)

(al/bind-keys
  :map bui-map
  ("↷" 'bui-history-back)
  ("↶" 'bui-history-forward))

(al/eval-after-load bui-list
  (al/bind-keys
    :map bui-list-mode-map
    ("→" 'bui-list-describe)
    ("i" 'bui-list-describe)
    ("z" 'bui-list-unmark)
    ("Z" 'bui-list-unmark-all)))

;;; bui.el ends here
