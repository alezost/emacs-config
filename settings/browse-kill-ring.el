;;; browse-kill-ring.el --- Settings for `browse-kill-ring' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'browse-kill-ring)
(require 'al-key)

(setq
 browse-kill-ring-separator (make-string 64 ?—)
 browse-kill-ring-separator-face nil)

(al/eval-at-hook browse-kill-ring-mode-hook
  ;; Key bindings are defined inside `browse-kill-ring-mode'.
  (al/bind-keys
    ("↑"   'browse-kill-ring-previous)
    ("↓"   'browse-kill-ring-forward)
    ("→"   'browse-kill-ring-insert-and-quit)
    ("M-d" 'browse-kill-ring-edit)))

;;; browse-kill-ring.el ends here
