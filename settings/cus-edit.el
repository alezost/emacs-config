;;; cus-edit.el --- Settings for `cus-edit' package  -*- lexical-binding: t -*-

(require 'cus-edit)
(require 'al-key)

(al/bind-keys-from-vars 'custom-mode-map 'al/widget-button-keys t)

(al/bind-keys
  :map custom-mode-map
  ("o" . Custom-goto-parent)
  ("g" . Custom-reset-standard))

;;; cus-edit.el ends here
