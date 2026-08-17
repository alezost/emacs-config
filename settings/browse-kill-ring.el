;;; browse-kill-ring.el --- Settings for `browse-kill-ring' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'browse-kill-ring)
(require 'al-key)

(defconst al/browse-kill-ring-keys
  '(("."   . browse-kill-ring-previous)
    ("e"   . browse-kill-ring-forward)
    ("u"   . browse-kill-ring-insert-and-quit)
    ("M-d" . browse-kill-ring-edit))
  "Alist of auxiliary keys for `browse-kill-ring-mode-map'.")

(setq
 browse-kill-ring-separator (make-string 64 ?—)
 browse-kill-ring-separator-face nil)

(al/eval-at-hook browse-kill-ring-mode-hook
  ;; Key bindings are defined inside `browse-kill-ring-mode'.
  (al/bind-keys-from-vars 'browse-kill-ring-mode-map
    'al/browse-kill-ring-keys t))

;;; browse-kill-ring.el ends here
