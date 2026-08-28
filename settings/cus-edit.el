;;; cus-edit.el --- Settings for `cus-edit' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'cus-edit)

(al/bind-keys
  :map custom-mode-map
  ("←" 'Custom-goto-parent)
  ("g" 'Custom-reset-standard))

;;; cus-edit.el ends here
