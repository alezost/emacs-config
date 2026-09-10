;;; ielm.el --- Settings for `ielm' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros)
  (require 'al-aux-macros))

(require 'ielm)
(require 'al-general)

(al/bind-keys
  :map ielm-map
  :parent (lisp-mode-shared-map comint-mode-map)
  ("RET" 'ielm-send-input))

(setq ielm-prompt "EL> ")

(al/call-at-hook ielm-mode-hook al/no-truncate-lines)

;;; ielm.el ends here
