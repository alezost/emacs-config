;;; cc-mode.el --- Settings for `cc-mode' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'cc-mode)

(al/bind-keys
  :map c-mode-base-map
  ("H-M-<tab>" 'c-indent-defun))

(setq
 c-default-style
 '((c-mode    . "stroustrup")
   (java-mode . "java")
   (awk-mode  . "awk")
   (other     . "gnu")))

;; Make "one-two-three" a symbol (useful for Emacs C source files).
(modify-syntax-entry ?- "_   " c-mode-syntax-table)

;;; cc-mode.el ends here
