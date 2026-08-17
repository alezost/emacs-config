;;; cc-mode.el --- Settings for `cc-mode' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key))

(require 'cc-mode)

(defconst al/c-base-keys
  '(("<H-M-tab>" . c-indent-defun)))
(al/bind-keys-from-vars 'c-mode-base-map
  '(al/prog-keys al/c-base-keys))

(setq
 c-default-style
 '((c-mode    . "stroustrup")
   (java-mode . "java")
   (awk-mode  . "awk")
   (other     . "gnu")))

;;; cc-mode.el ends here
