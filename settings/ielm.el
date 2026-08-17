;;; ielm.el --- Settings for `ielm' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'ielm)
(require 'al-key)

(defconst al/ielm-keys
  '("C-j"
    ("RET" . ielm-send-input)))

(al/bind-keys-from-vars 'ielm-map
  '(al/lisp-shared-keys al/comint-keys al/ielm-keys))

(setq ielm-prompt "EL> ")

(al/call-at-hook ielm-mode-hook al/no-truncate-lines)

;;; ielm.el ends here
