;;; cc-mode.el --- Settings for `cc-mode' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key))

(require 'cc-mode)
(require 'al-general)

(al/bind-keys
  :map c-mode-base-map
  ("<H-M-tab>" . c-indent-defun))

(setq
 c-default-style
 '((c-mode    . "stroustrup")
   (java-mode . "java")
   (awk-mode  . "awk")
   (other     . "gnu")))

;;; cc-mode.el ends here
