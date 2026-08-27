;;; cus-edit.el --- Settings for `cus-edit' package  -*- lexical-binding: t -*-

(require 'cus-edit)
(require 'al-key)

(al/bind-keys
  :map custom-mode-map
  ("←" 'Custom-goto-parent)
  ("g" 'Custom-reset-standard))

;;; cus-edit.el ends here
