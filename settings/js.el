;;; js.el --- Settings for `js' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros)
  (require 'al-key))

(require 'js)
(require 'al-general)

(al/bind-keys
  :map js-mode-map
  ("M-d"     . js-find-symbol)
  ("C-c M-v" . js-eval)
  ("C-M-v"   . js-eval-defun))

(al/eval-at-hook js-mode-hook
  (setq-local al/delimiter
              (concat (make-string 64 ?/) "\n///")))

;;; js.el ends here
