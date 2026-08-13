;;; nxml-mode.el --- Settings for `nxml-mode' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'nxml-mode)
(require 'al-key)

(defconst al/nxml-keys
  '(("C-M-." . nxml-backward-up-element)
    ("C-M-e" . nxml-down-element)
    ("C-M-o" . nxml-backward-element)
    ("C-M-u" . nxml-forward-element)))

(al/bind-keys-from-vars 'nxml-mode-map 'al/nxml-keys)

(al/eval-at-hook nxml-mode-hook
  (rng-validate-mode 0))

;;; nxml-mode.el ends here
