;;; ibuffer.el --- Settings for `ibuffer' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'ibuffer)
(require 'al-general)
(require 'al-key)

(defconst al/ibuffer-keys
  '(("u"   . ibuffer-visit-buffer)
    ("."   . ibuffer-backward-line)
    ("e"   . ibuffer-forward-line)
    ("M-." . ibuffer-backward-filter-group)
    ("M-e" . ibuffer-forward-filter-group)

    ("d"   . ibuffer-visit-buffer-other-window-noselect)
    ("C-d" . ibuffer-visit-buffer-other-window)
    ("C-l"   (ibuffer-update t))

    ("M"   . ibuffer-mark-unsaved-buffers)
    ("z"   . ibuffer-unmark-forward)
    ("Z"     (ibuffer-unmark-all ?\r))
    ("s r" . ibuffer-do-sort-by-recency)
    ("* o" . ibuffer-mark-old-buffers))
  "Alist of auxiliary keys for `ibuffer-mode-map'.")

(al/bind-keys-from-vars 'ibuffer-mode-map 'al/ibuffer-keys)

(setq ibuffer-default-sorting-mode 'recency)

(al/call-at-hook ibuffer-mode-hook
  al/mode-ibuffer-info
  hl-line-mode)

;;; ibuffer.el ends here
