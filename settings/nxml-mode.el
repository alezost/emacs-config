;;; nxml-mode.el --- Settings for `nxml-mode' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'nxml-mode)
(require 'al-key)

(al/bind-keys
  :map nxml-mode-map
  ("C-M-↑" 'nxml-backward-up-element)
  ("C-M-↓" 'nxml-down-element)
  ("C-M-←" 'nxml-backward-element)
  ("C-M-→" 'nxml-forward-element))

(al/eval-at-hook nxml-mode-hook
  (rng-validate-mode 0))

;;; nxml-mode.el ends here
